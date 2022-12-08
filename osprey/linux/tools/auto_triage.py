#!/usr/bin/python3
'''
auto_triage.py is designed to auto triage compile/link/run/option failures.
1. compile failure
   generate a small reproducer
2. link failure
   report bad object files
3. run failure
   report bad files
4. option failure
   report bad compiler options and functions
'''
import os
import argparse
import logging
import subprocess
import re
import sys

SW_KP_OPT = "-sw -kp"
CGOFF = "-CG:optimization_level=0"
PREOPTOFF = "-PHASE:p=0"
WOPTOFF = "-PHASE:w=0"
LNOOFF = "-PHASE:l=0"
CG_SKIP = "-CG:skip_e=#BVALUE# -CG:skip_a=#AVALUE# -CG:skip_b=#BVALUE# "
OPT_SKIP = "-OPT:skip_e=#BVALUE# -OPT:skip_a=#AVALUE# -OPT:skip_b=#BVALUE#"
CG_OPTIONS = ["-CG:cflow=0", "-CG:cflow_unreachable=0", CG_SKIP]
OPT_OPTIONS = [OPT_SKIP]

OPT_LEVEL = ["-O0", "-O1", "-O2", "-O3"]
PHASE_O0 = [""]
PHASE_O1 = [CGOFF]
PHASE_O2 = [CGOFF, WOPTOFF, PREOPTOFF]
PHASE_O3 = [CGOFF, LNOOFF, WOPTOFF, PREOPTOFF]

PHASE_OPTIONS = [PHASE_O0, PHASE_O1, PHASE_O2, PHASE_O3]
SUB_OPTION_MAPPING = {CGOFF: CG_OPTIONS, WOPTOFF:OPT_OPTIONS}

def get_work_list():
    '''
    read work list form input file
    '''
    global WORK_LIST
    inputfile = args.input
    input_fp = open(inputfile, "r")
    WORK_LIST = input_fp.read().splitlines()
    input_fp.close()

def do_compile_reproduce():
    '''
    reproduce for compile issue
    '''
    cmd = get_exec_cmd()
    cmd[len(cmd)-1] = "cvise " + cmd[len(cmd) - 1]
    run_cmd(cmd, 10000)

def main():
    '''
    Main Entry
    '''
    parser = argparse.ArgumentParser(description='Auto Triage',
                                     formatter_class=argparse.RawTextHelpFormatter)
    parser.add_argument('-d', '--debug', action='store_true', default=False,
                        help='Debug mode')
    parser.add_argument('-k', '--keep', action='store_true', default=False,
                        help='Keep working dir')
    parser.add_argument('-t', '--type', metavar='[COMPILE|LINK|RUN|OPTION]', required=True,
                        help='''Reproduce type:
COMPILE: reproduce with a small source file
LINK:    reproduce with a samll list of object files, if possible continue reproduce with source
RUN:     reproduce with a small list of bad object files, and try to find the bad compile options
OPTION:  reproduce by find the compile options and error functions'''
                       )
    parser.add_argument('-gdir', '--gdir', metavar='GOOD_PATH',
                        help='For run time reproducer only, good objects dir')
    parser.add_argument('-bdir', '--bdir', metavar='BAD_PATH',
                        help='For run time reproducer only, bad objects dir')
    parser.add_argument('-i', '--input', help='input candidate file', required=True)
    parser.add_argument('-s', '--script', help='verify script', required=True)

    global args
    args = parser.parse_args()

    if args.debug is True:
        loglevel = logging.DEBUG
    else:
        loglevel = logging.INFO
    logformat = "%(message)s"
    logging.basicConfig(level=loglevel, format=logformat)

    get_work_list()

    if args.type == "COMPILE":
        do_compile_reproduce()
    elif args.type == "LINK":
        do_link_reproduce()
    elif args.type == "RUN":
        if args.bdir is None:
            logging.error("bad dir is required for run")
            sys.exit(1)
        if args.gdir is None:
            logging.error("good dir is required for run")
            sys.exit(1)
        do_run_reproduce()
    elif args.type == "OPTION":
        do_option_triage()

def get_exec_cmd(work_list=None):
    if work_list is None:
        work_list = WORK_LIST
    cmd = ['/usr/bin/bash', '-c']
    cmd.append(args.script + " " +    (" ").join(work_list))
    return cmd

def add_option(cmd, option):
    last_idx = len(cmd) - 1
    cmd[last_idx] = cmd[last_idx] + (" ") + option

def run_cmd(cmd, timeout=100, outf=None):
    '''
    run subprocess, return CompletedProcess object
    return code is in result.returncode
    '''
    logging.debug("Run_cmd : { %s }", (" ").join(cmd))
    if outf is not None:
        outfp = open(outf, "w")
    else:
        outfp = subprocess.PIPE
    try:
        result = subprocess.run(cmd, stdout=outfp, stderr=subprocess.PIPE, timeout=timeout)
        if result.returncode != 0:
            logging.info("Not Reproduced - { %s }", (" ").join(cmd))
            logging.debug(result.stderr.decode('utf-8').replace('\n', ''))
            logging.debug(result.stdout.decode('utf-8').replace('\n', ''))
        else:
            logging.info("Reproduced - { %s }\n", (" ").join(cmd))
    except Exception as e:
        result = subprocess.CompletedProcess(cmd, 1)
        logging.error("Exec cmd Failed with exception: { %s }:%s", (" ").join(cmd), str(e))
    return result

def triage_opt_level():
    '''
    returns opt level that first reproduced the error
    '''
    idx = 0
    for level in OPT_LEVEL:
        cmd = get_exec_cmd()
        add_option(cmd, level)
        logging.debug(cmd)
        ret = run_cmd(cmd)
        if ret.returncode == 0:
            return idx
        idx = idx + 1
    return idx

def triage_phase_options(opt_level, phase_options):
    '''
    find the error phase by turn off phase options
    '''
    for option_group in phase_options:
        cmd = get_exec_cmd()
        add_option(cmd, opt_level)
        add_option(cmd, option_group)
        ret = run_cmd(cmd)
        if ret.returncode != 0:
            # turn off option eliminate the error
            return option_group
    return None

def get_before_after(opt_level, skip_values):
    '''
    get the compile unit numbers from compile output
    '''
    cmd = get_exec_cmd()
    add_option(cmd, opt_level)
    add_option(cmd, SW_KP_OPT)
    ret = run_cmd(cmd)
    output = ret.stderr.decode('utf-8')
    for line in reversed(output.splitlines()):
        if line.startswith("Compiling "):
            pattern = re.compile(r'[(](.*)[)]', re.S)
            skip_values["before"] = int(re.findall(pattern, line)[0])
            skip_values["after"] = 0
            break

def triage_sub_options(opt_level, phase_option):
    '''
    find the sub options can eliminate the error
    '''
    sub_options = SUB_OPTION_MAPPING.get(phase_option)
    if sub_options is not None:
        for sub_option in sub_options:
            if sub_option in (CG_SKIP, OPT_SKIP):
                skip_values = {"before" :-1, "after" : -1}
                get_before_after(opt_level, skip_values)
                if skip_values["before"] == -1 or skip_values["after"] == -1:
                    logging.error("Invalid skip_values")
                    break
                search_option = opt_level + " " + sub_option
                final_range = []
                do_binary_search(skip_values["after"],
                                 skip_values["before"],
                                 final_range,
                                 search_option)
                logging.info("Final range is: %s", final_range)
            else:
                new_cmd = get_exec_cmd()
                add_option(new_cmd, opt_level)
                add_option(new_cmd, sub_option)
                ret = run_cmd(new_cmd)
                if ret.returncode != 0:
                    logging.info("Find the Error option: %s", sub_option)

def do_option_triage():
    level = triage_opt_level()
    if level == 0:
        logging.info("case failed at O0: may be caused by case it self")
    elif level >= len(OPT_LEVEL):
        logging.info("Not reproduceable at all level, check your script")
    else:
        phase_options = PHASE_OPTIONS[level]
        phase_option = triage_phase_options(OPT_LEVEL[level], phase_options)
        if phase_option is None:
            logging.info("Unable to find any phase option can eliminate the error")
        else:
            triage_sub_options(OPT_LEVEL[level], phase_option)

def update_cmd(low, mid, high, option_str=None):
    if args.type == "RUN":
        new_wl = []
        new_wl.extend(WORK_LIST)
        idx = 0

        while idx < len(WORK_LIST):
            if (idx < mid) and (idx >= low):
                new_wl[idx] = args.bdir + "/" + WORK_LIST[idx]
            else:
                new_wl[idx] = args.gdir + "/" + WORK_LIST[idx]
            idx = idx + 1
        return get_exec_cmd(new_wl)
    elif args.type == "OPTION":
        cmd = get_exec_cmd()
        option_str = option_str.replace("#BVALUE#", str(mid))
        option_str = option_str.replace("#AVALUE#", str(low))
        add_option(cmd, option_str)
        return cmd
    else:
        logging.error("not implemented")
        sys.exit(1)
        return None

def process_search_result(ret, low_high_res, final_res):
    cont = True
    if args.type == "RUN":
        if ret == 0:
            # recover to good objects
            final_res.clear()
            for recov_idx in range(low_high_res[0], low_high_res[1], 1):
                final_res.append(WORK_LIST[recov_idx])
            low_high_res[2] = low_high_res[1]    # high = mid
        else:
            logging.debug("cont")
            low_high_res[0] = low_high_res[1] # low = mid
            cont = False
    elif args.type == "OPTION":
        if ret == 0:
            # turn off option still reproduced
            low_high_res[0] = low_high_res[1] # low = mid
            cont = False
        else:
            final_res.clear()
            final_res.append(low_high_res[0])
            final_res.append(low_high_res[1])
            low_high_res[2] = low_high_res[1]    # high = mid
    else:
        logging.error("not implemented")
        sys.exit(1)
    return cont


def do_binary_search(low, high, final_res, option_str=None):
    logging.info("Binary search with : [%d-%d]", low, high)
    if high <= low:
        return
    mid = int((high - low) / 2) + low
    if mid == low:
        return
    cmd = update_cmd(low, mid, high, option_str)
    ret = run_cmd(cmd).returncode
    low_high_res = [low, mid, high]
    cont = process_search_result(ret, low_high_res, final_res)
    if cont is False:
        cmd = update_cmd(mid, high, high, option_str)
        ret = run_cmd(cmd).returncode
        low_high_res = [mid, high, high]
        cont = process_search_result(ret, low_high_res, final_res)

    do_binary_search(low_high_res[0], low_high_res[1], final_res, option_str)

def update_work_list(finalobjs, rnd):
    if len(finalobjs) == 0:
        return
    new_bdir = args.bdir + "_" + str(rnd)
    os.system("rm -rf " + new_bdir)
    os.system("cp -r " + args.bdir + " " + new_bdir)
    for item in finalobjs:
        os.system("cp " + args.gdir + "/" + item + " " + new_bdir)
    args.bdir = new_bdir

def do_run_reproduce():
    '''
    binary search the bad objects
    low--mid--high
    S1:replace low-mid with bad objects
    if final run success, set low to mid
    if final run fails, set mid to high
    continue S1
    '''
    logging.info(WORK_LIST)
    low = 0
    high = len(WORK_LIST) - 1
    round_idx = 0
    result = []
    finalobjs = []

    while 1:
        finalobjs = []
        do_binary_search(low, high, finalobjs)
        if len(finalobjs) == 0:
            break
        result.append(finalobjs)
        round_idx = round_idx + 1
        update_work_list(finalobjs, round_idx)
        logging.info("Final bad objects of round %d : %s", round_idx, str(finalobjs))

    for i in result:
        logging.info("Final bad objects: %s", str(i))

def do_link_reproduce():
    '''
    entry for link failure reproducer
    '''
    last_success = WORK_LIST
    worklen = len(WORK_LIST)
    piece = int(worklen / 2)
    while piece > 0:
        idx = 0
        fullobjs = []
        fullobjs.extend(WORK_LIST)
        while idx < worklen:
            remidx = 0
            remlist = []
            while remidx < piece and idx < worklen:
                remlist.append(fullobjs[idx])
                WORK_LIST.remove(fullobjs[idx])
                idx = idx + 1
                remidx = remidx + 1

            cmd = " ".join(WORK_LIST)
            cmd = args.script + " "+ cmd
            logging.info(cmd)
            ret = os.system(cmd)
            msg = " ".join(remlist)
            msg = "Try remove objs" + msg
            if ret == 0:
                logging.info("%s: failed", msg)
                WORK_LIST.extend(remlist)
            else:
                logging.info("%s: success", msg)
                last_success = WORK_LIST
            logging.info("Final reduced objs with piece %d:", piece)
            logging.info(last_success)
            logging.info("Command:")
            logging.info("./exec.sh %s", " ".join(last_success))
            outfile = "delta_list" + str(piece)
            out_fp = open(outfile, "w")
            out_fp.write("\n".join(last_success))
            out_fp.close()
            piece = int(piece / 2)

if __name__ == "__main__":
    main()
