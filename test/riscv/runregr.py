#!/usr/bin/python3
import os
import sys
import logging
import argparse
import tempfile
import subprocess

CC = "xcalcc"
CXX = "xcalc++"
OPTION = "-c"
SCRIPT_DIR = os.path.abspath(os.path.dirname(sys.argv[0]))
REG_PATH = SCRIPT_DIR + "/regression"
WORK_DIR = tempfile.TemporaryDirectory(suffix=".workdir", prefix="reg.", dir=SCRIPT_DIR)

def get_tests(test_list):
    '''
    Get all test cases
    '''
    basefiles = []
    backup_list = []
    backup_list.extend(test_list)
    for item in backup_list:
        if not os.path.exists(item):
            basefiles.append(os.path.basename(item))
            test_list.remove(item)
    orig_len = len(basefiles)

    for (dirpath, dirnames, filenames) in os.walk(REG_PATH):
        for case in filenames:
            if orig_len != 0:
                if case in basefiles:
                    test_list.append(dirpath + "/" + case)
            else:
                test_list.append(dirpath + "/" + case)

def get_exec_cmd(test):
    '''
    Get the execution commands
    '''
    cmd = []
    output = os.path.basename(test) + ".o"
    if test.endswith(".c"):
        cmd.append(CC)
    elif test.endswith(".cxx") or test.endswith(".cpp") \
         or test.endswith(".cc"):
        cmd.append(CXX)
    else:
        return None
    cmd.append(OPTION)
    cmd.append(test)
    cmd.append("-o")
    cmd.append(output)
    return cmd

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
            logging.debug("Exec Error - { %s }", (" ").join(cmd))
            logging.debug(result.stderr.decode('utf-8').replace('\n', ''))
            logging.debug(result.stdout.decode('utf-8').replace('\n', ''))
        else:
            logging.debug("Exec Success - { %s }\n", (" ").join(cmd))
    except Exception as e:
        result = subprocess.CompletedProcess(cmd, 1)
        logging.error("Exec cmd Failed with exception: { %s }:%s", (" ").join(cmd), str(e))
    return result

def report_result(result):
    '''
    Report testing results
    '''
    pass_cnt = 0
    logging.info("Riscv Regression Test Result")
    logging.info("============================================")
    for item in result:
        short_name = os.path.basename(item)
        if result[item] == 0:
            logging.info("{0:30}| PASS".format(short_name))
            pass_cnt = pass_cnt + 1
        else:
            logging.info("{0:30}| FAIL".format(short_name))
    logging.info("============================================")
    logging.info("Total Pass: %d/%d", pass_cnt, len(result))

def run_regression(tests):
    '''
    Driver for run regression test
    '''
    test_result = {}
    if tests is None:
        tests = []
    get_tests(tests)
    os.chdir(WORK_DIR.name)
    for test in tests:
        cmd = get_exec_cmd(test)
        ret = run_cmd(cmd)
        test_result[test] = ret.returncode

    report_result(test_result)

def main():
    '''
    Main Entry
    '''
    parser = argparse.ArgumentParser(description='Run Regression')
    parser.add_argument('-t', '--tests', nargs='+', help='Run given tests')
    parser.add_argument('-k', '--keep', action='store_true', help='Keep workarea',
                        default=False)
    parser.add_argument('-d', '--debug', action='store_true', default=False,
                        help='Debug mode')
    args = parser.parse_args()
    if args.debug is True:
        loglevel = logging.DEBUG
    else:
        loglevel = logging.INFO
    logformat = "%(message)s"
    logging.basicConfig(level=loglevel, format=logformat)

    run_regression(args.tests)

    if args.keep is True:
        logging.info("Saved Working Dir at: %s.keep", WORK_DIR.name)
        os.rename(WORK_DIR.name, WORK_DIR.name + ".keep")

if __name__ == "__main__":
    main()
