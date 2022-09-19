#!/usr/bin/python3

#  Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.

'''
Conver intent table in excel into archive
'''

import sys
import os
import shutil
import argparse
import logging
import subprocess
import re
import xlrd

# index
ID_IDX = 0
SIGNATURE_IDX = 1
MAJOR_TYPE_IDX = 5
API_IDX = MAJOR_TYPE_IDX + 1
ERRCODE_IDX = API_IDX + 1
OPND0_TYPE_IDX = ERRCODE_IDX + 1
OPND0_IDX = OPND0_TYPE_IDX + 1
OPND1_TYPE_IDX = OPND0_IDX + 1
OPND1_IDX = OPND1_TYPE_IDX + 1
OPND2_TYPE_IDX = OPND1_IDX + 1
OPND2_IDX = OPND2_TYPE_IDX + 1
OPND3_TYPE_IDX = OPND2_IDX + 1
OPND3_IDX = OPND3_TYPE_IDX + 1
OPND4_TYPE_IDX = OPND3_IDX + 1
OPND4_IDX = OPND4_TYPE_IDX + 1
OPND5_TYPE_IDX = OPND4_IDX + 1
OPND5_IDX = OPND5_TYPE_IDX + 1
OPND6_TYPE_IDX = OPND5_IDX + 1
OPND6_IDX = OPND6_TYPE_IDX + 1

# const data
MAJOR_TYPE_LIST = ['RBC_EVAL', 'RBC_MODEL', 'RBC_ASSERT', 'RBC_FSM']
PARSABLE_PARAM_TUPLE = ("IET_IET", "IET_CONST", "IET_STRING_TAG")
DEFAULT_INDENT = 4
FUNC_START_POS = 2 * DEFAULT_INDENT


class JavaSig:
    '''
    <java.lang.Runtime: java.io.InputStream getLocalizedInputStream(java.io.InputStream)>
    <io.xc5.cert.j_env03_0: void <init>(java.net.URL[])>
    <io.xc5.cert.j_env03_0: boolean checkRtPermission(java.net.URL[],java.lang.String)>
    '''
    def __init__(self, sig):
        clazz_matcher = re.search(r'(?<=<).*(?=:)', sig)
        assert (clazz_matcher is not None), "Can't match class name, sigature: %s" % sig
        self._sig = sig
        self._clazz_name = clazz_matcher.group(0)
        method_sig_matcher = re.search(r'(?<=: ).*(?=>)', sig)
        assert (
            method_sig_matcher is not None), "Can't match method signature, sigature: %s" % sig
        method_sig = method_sig_matcher.group(0)
        ret_matcher = re.search(r'^.*(?= )', method_sig)
        assert (
            ret_matcher is not None), "Can't match return type, method sigature: %s" % method_sig
        self._ret_type = ret_matcher.group(0)
        met_name_matcher = re.search(r'(?<= ).*(?=\()', method_sig)
        assert (
            met_name_matcher is not None), "Can't match method name, method sigature: %s" % method_sig
        self._met_name = met_name_matcher.group(0)
        args_matcher = re.search(r'(?<=\().*(?=\))', method_sig)
        assert (
            args_matcher is not None), "Can't match arg list, method sigature: %s" % method_sig
        self._args = args_matcher.group(0).split(',')
        interface_matcher = re.search(r'^(I)', sig)
        if (interface_matcher):
            self.interface_flag = True
        else:
            self.interface_flag = False

    def clazz_name(self):
        '''Class name getter'''
        return self._clazz_name

    def ret_type(self):
        '''Return type getter'''
        return self._ret_type

    def met_name(self):
        '''Method name getter'''
        return self._met_name

    def args(self):
        '''Argument list getter'''
        return self._args

    def sig(self):
        '''Original signature getter'''
        return self._sig

    def is_interface(self):
        '''Is interface method, used for adding the default modifier'''
        return self.interface_flag

    def __eq__(self, other):
        '''Use sig eq instead'''
        return self._sig.__eq__(other.sig())

    def __ne__(self, other):
        '''Use sig eq instead'''
        return self._sig.__ne__(other.sig())

    def __hash__(self):
        '''Use sig hash instead'''
        return self._sig.__hash__()

    def __str__(self):
        '''Use sig str instead'''
        return self._sig

    def __unicode__(self):
        '''Use sig unicode instead'''
        return self._sig

    def __repr__(self):
        '''Use sig repr instead'''
        return self._sig


def dump_entry(sig, mtype, api, errcode, tp0, op0, tp1, op1,
               tp2, op2, tp3, op3, tp4, op4, tp5, op5, tp6, op6):
    '''
    Print out intent table entry, for debug
    '''
    if logging.getLogger().level != logging.DEBUG:
        return
    line = "['" + str(sig) + "', '" + mtype + "', '" + api
    if errcode != "":
        line += "', 'ERROR':'" + errcode
    if tp0 in PARSABLE_PARAM_TUPLE:
        if type(op0) is int:
            opnd = str(int(op0))
        else:
            opnd = op0
        line += "', '" + tp0 + "':'" + opnd
    if tp1 in PARSABLE_PARAM_TUPLE:
        if type(op1) is int:
            opnd = str(int(op1))
        else:
            opnd = op1
        line += "', '" + tp1 + "':'" + opnd
    if tp2 in PARSABLE_PARAM_TUPLE:
        if type(op2) is int:
            opnd = str(int(op2))
        else:
            opnd = op2
        line += "', '" + tp2 + "':'" + opnd
    if tp3 in PARSABLE_PARAM_TUPLE:
        if type(op3) is int:
            opnd = str(int(op3))
        else:
            opnd = op3
        line += "', '" + tp3 + "':'" + opnd
    if tp4 in PARSABLE_PARAM_TUPLE:
        if type(op4) is int:
            opnd = str(int(op4))
        else:
            opnd = op4
        line += "', '" + tp4 + "':'" + opnd
    if tp5 in PARSABLE_PARAM_TUPLE:
        if type(op5) is int:
            opnd = str(int(op5))
        else:
            opnd = op5
        line += "', '" + tp5 + "':'" + opnd
    if tp6 in PARSABLE_PARAM_TUPLE:
        if type(op6) is int:
            opnd = str(int(op6))
        else:
            opnd = op6
        line += "', '" + tp6 + "':'" + opnd
    line += "']"
    logging.debug(line)

def write_ref(out_file, ref_map, op0, indent):
    '''
    Write out the reference entry
    '''
    if not op0.isdigit():
        logging.error("Invalid reference id")
        sys.exit(-1)
    row = ref_map.get(int(op0))
    if row is None:
        logging.error("null entry with id %s", op0)
        sys.exit(-1)
    out_file.write("\n")
    write_entry(ref_map, out_file, row[SIGNATURE_IDX], row, indent + DEFAULT_INDENT)

def write_arg(out_file, ref_map, tp0, op0, indent):
    '''
    Write out intent table entry operand
    '''
    if tp0 == "IET_IET":
        if isinstance(op0, int):
            out_file.write(str(op0))
        elif isinstance(op0, float):
            out_file.write(str(op0))
        elif op0 == "THIS":
            out_file.write("RBC_ENGINE.Get_this_pointer()")
        elif op0 == "RET":
            out_file.write("RBC_ENGINE.Get_ret()")
        elif op0.casefold() == "true":
            out_file.write("1")
        elif op0.casefold() == "false":
            out_file.write("0")
        elif op0[0:3] == "ARG":
            out_file.write("RBC_ENGINE.Get_arg(" + op0[3] + ")")
        elif op0.isdigit():
            write_ref(out_file, ref_map, op0, indent)
    elif tp0 == "IET_CONST":
        if isinstance(op0, int) or isinstance(op0, float):
            out_file.write(str(op0))
        elif isinstance(op0, str):
            out_file.write('"%s"' % str(op0))
    elif tp0[0:10] == "IET_STRING":
        out_file.write("\"" + op0 + "\"")


def write_return(out_file, ret_type):
    '''
    Write out return statement
    '''
    if ret_type in ('byte', 'short', 'int', 'long',
                    'float', 'double', 'char'):
        write_indent(out_file, FUNC_START_POS)
        out_file.write("return 0;\n")
    elif ret_type == "boolean":
        write_indent(out_file, FUNC_START_POS)
        out_file.write("return false;\n")
    elif ret_type != "void":
        write_indent(out_file, FUNC_START_POS)
        out_file.write("return null;\n")


def write_comma(out_file, tp0):
    '''
    Write out ',' between arguements
    '''
    if tp0 == "IET_IET" or tp0 == "IET_CONST" or tp0[0:10] == "IET_STRING":
        out_file.write(", ")


def write_func_sig(out_file, sig:JavaSig):
    '''
    Write out function signature, a signature is of form:
    # <class_name: return_type func_name(args)>
    # <java.lang.Runtime: java.io.InputStream getLocalizedInputStream(java.io.InputStream)>
    '''
    func_name = sig.met_name()
    clazz_name = sig.clazz_name()
    ret_type = sig.ret_type()
    # process constructor signature
    if func_name.find("<init>") != -1:
        names = clazz_name.split(".")
        func_name = " " + names[len(names) - 1]
        ret_type = ""
    func_name += "("
    i = 1
    items = sig.args()
    if len(items) == 1:
        arg = items[0]
        if arg in ("void", ""):
            func_name += arg
        else:
            func_name += arg + " arg1"
    else:
        for arg in items:
            if arg in ("void", ""):
                func_name += arg
            else:
                func_name += arg + " arg" + str(i) + ", "
                i = i + 1
        func_name = func_name.rstrip(", ")
    func_name += ")"
    modifiers = "public"
    if sig.is_interface():
        modifiers = "default"
    if ret_type != "":
        out_file.write('\n    %s %s %s {\n' % (modifiers, ret_type, func_name))
    else:
        out_file.write('\n    %s %s {\n' % (modifiers, func_name))

def write_indent(outfile, indent):
    ''' write indent '''
    cnt = (int)(indent / DEFAULT_INDENT)
    for idx in range(0, cnt):
        outfile.write('    ')

def write_entry(ref_map, out_file, sig, row, indent):
    '''
    Write out an intent table entry
    '''
    major_type = row[MAJOR_TYPE_IDX]
    api = row[API_IDX]
    err = row[ERRCODE_IDX]
    op0 = convert_float(row[OPND0_IDX])
    tp0 = row[OPND0_TYPE_IDX]
    op1 = convert_float(row[OPND1_IDX])
    tp1 = row[OPND1_TYPE_IDX]
    op2 = convert_float(row[OPND2_IDX])
    tp2 = row[OPND2_TYPE_IDX]
    op3 = convert_float(row[OPND3_IDX])
    tp3 = row[OPND3_TYPE_IDX]
    op4 = convert_float(row[OPND4_IDX])
    tp4 = row[OPND4_TYPE_IDX]
    op5 = convert_float(row[OPND5_IDX])
    tp5 = row[OPND5_TYPE_IDX]
    op6 = convert_float(row[OPND6_IDX])
    tp6 = row[OPND6_TYPE_IDX]

    # print out entry info for debugging
    dump_entry(sig, major_type, api, err, tp0, op0,
               tp1, op1, tp2, op2, tp3, op3,
               tp4, op4, tp5, op5, tp6, op6)

    if major_type not in MAJOR_TYPE_LIST:
        logging.debug("%s is not an RBC entry.", sig)
        return
    write_indent(out_file, indent)
    if major_type == "RBC_MODEL":
        out_file.write("RBC_ENGINE.Model_decl(\n")
        indent = indent + DEFAULT_INDENT
        write_indent(out_file, indent)
    if major_type == "RBC_ASSERT":
        tp1 = "IET_STRING_ERRCODE"
        op1 = err
    out_file.write("RBC_ENGINE." + api + "(")
    write_arg(out_file, ref_map, tp0, op0, indent)
    write_comma(out_file, tp1)
    write_arg(out_file, ref_map, tp1, op1, indent)
    write_comma(out_file, tp2)
    write_arg(out_file, ref_map, tp2, op2, indent)
    write_comma(out_file, tp3)
    write_arg(out_file, ref_map, tp3, op3, indent)
    write_comma(out_file, tp4)
    write_arg(out_file, ref_map, tp4, op4, indent)
    write_comma(out_file, tp5)
    write_arg(out_file, ref_map, tp5, op5, indent)
    write_comma(out_file, tp6)
    write_arg(out_file, ref_map, tp6, op6, indent)
    out_file.write(")")
    if major_type == "RBC_ASSERT":
        out_file.write(";\n")
    if major_type == "RBC_MODEL":
        out_file.write(");\n")


def compile_rbc_engine(rbc_engine_path):
    '''
    Compile RBC_ENGINE.java required in generating rules
    '''
    subprocess.run(['javac', '-d', '.', rbc_engine_path], check=True)


def read_table(intent_sheet, ref_map:dict, interface_map:dict):
    '''
    Read the data from intent table sheet, return class_map and ref_map
    1. class_map: the following description descripe the orgnization of the returned class map
    {
        # a map of class to functions with rules lists
        'class_name': {
            # a map of function signature to rules lists on it
            'signature' : [row, row]
        }
        i.e.
        'java.lang.Runtime' : {
            # class name is required in signature because all constructors are of the same name '<init>'
            'java.lang.Runtime: java.io.InputStream getLocalizedInputStream(java.io.InputStream)' : [row, row]
        }
    }
    2. ref_map: defines the mapping of ID -> row, it will be used for generate RBC_EVAL referenced entry
    '''
    class_map = {}
    # write out intent table entries
    for index in range(1, intent_sheet.nrows):
        row = intent_sheet.row_values(index)
        signature = row[SIGNATURE_IDX]
        major_type = row[MAJOR_TYPE_IDX]
        if signature is None:
            continue
        if signature == "":
            continue
        if major_type not in MAJOR_TYPE_LIST:
            continue

        # Assuming this is a method signature, TODO: support type names
        sig = JavaSig(signature)
        if (sig.is_interface()):
            interface_map[sig.clazz_name()] = True

        if major_type == "RBC_EVAL":
            idx = int(row[ID_IDX])
            ref_map[idx] = row
        else:
            # a signature in intent table is of form
            # <class_name: return_type func_name(args)>
            # <java.lang.Runtime: java.io.InputStream getLocalizedInputStream(java.io.InputStream)>
            class_name = sig.clazz_name()
            if class_name in class_map:
                sig_map = class_map[class_name]
                if sig in sig_map:
                    sig_map[sig].append(row)
                else:
                    sig_map[sig] = [row]
            else:
                sig_map = {}
                sig_map[sig] = [row]
                class_map[class_name] = sig_map
    return class_map


def is_java_builtin_type(java_type):
    '''Determine a java type is java primitive type or not'''
    builtin_type = ('byte', 'short', 'int', 'long',
                    'float', 'double', 'char', 'boolean', 'void', '')
    if '.' in java_type:
        return False
    for builtin in builtin_type:
        if java_type.startswith(builtin):
            return True
    return False


def collect_dep_class(class_map, dep_class_set, sig):
    '''
    Should collect depencency classes that from the signature,
    javac require those dependencies, otherwize javac can't compile
    '''
    java_type = sig.ret_type().replace('[]', '')
    args = sig.args()
    if not is_java_builtin_type(java_type):
        if java_type not in class_map:
            dep_class_set.add(java_type)
    for arg_type in args:
        java_type = arg_type.replace('[]', '')
        if not is_java_builtin_type(java_type):
            if java_type not in class_map:
                dep_class_set.add(java_type)


def create_file_by_class_name(clazz_name):
    '''Given a class name, create a java file, have the right package position'''
    file_path = clazz_name.replace(".", os.sep)
    file_path = file_path + ".java"
    dir_name = os.path.dirname(file_path)
    if dir_name != "" and not os.path.exists(dir_name):
        os.makedirs(dir_name)
    out_file = open(file_path, "w")
    out_file.close()
    return file_path


def dump_class_header(out_file, clazz_name:str, dump_import:bool, interface_map:dict):
    '''Dump the rbc class header, including the left brace'''
    index = clazz_name.rfind('.')
    clazz_only_name = clazz_name
    if index != -1:
        package = clazz_name[:index]
        clazz_only_name = clazz_name[index + 1:]
        out_file.write("package " + package + ";\n")
    if dump_import:
        out_file.write("import io.xc5.RBC_ENGINE;\n\n")

    if interface_map.get(clazz_name) is not None:
        def_line = "public interface " + clazz_only_name + " {\n"
    else:
        def_line = "public class " + clazz_only_name + " {\n"

    # write out class name
    out_file.write(def_line)


def dump_dep_class_to_java(dep_class_set, interface_map):
    '''Dump the depencence classes into files, only package and class name are required'''
    dep_class_files = []
    for clazz_name in dep_class_set:
        file_path = create_file_by_class_name(clazz_name)
        dep_class_files.append(file_path)
        out_file = open(file_path, "w")
        logging.info("Dump dependency dummy %s ...", file_path)
        dump_class_header(out_file, clazz_name, False, interface_map)
        out_file.write("}\n")
        out_file.close()
    return dep_class_files

def convert_float(opnd):
    '''Python read excel numbers as float, convert them to int'''
    if isinstance(opnd, float):
        return str(int(opnd))
    return opnd

def convert_intent_table_to_java(intent_sheet):
    '''
    Convert excel table rows into java sources
    '''
    src_files = []
    dep_class_set = set()
    ref_map = {}
    interface_map = {}
    class_map = read_table(intent_sheet, ref_map, interface_map)
    for clazz_name in class_map:
        file_path = create_file_by_class_name(clazz_name)
        src_files.append(file_path)
        out_file = open(file_path, "w")
        logging.info("Dump rule file %s ...", file_path)
        dump_class_header(out_file, clazz_name, True, interface_map)
        signature_map = class_map[clazz_name]
        for sig in signature_map:
            collect_dep_class(class_map, dep_class_set, sig)
            # write function header first
            write_func_sig(out_file, sig)
            row_list = signature_map[sig]
            for row in row_list:
                write_entry(ref_map, out_file, sig, row, FUNC_START_POS)
            # end for row in row_list
            write_return(out_file, sig.ret_type())
            # right brace of method body
            write_indent(out_file, DEFAULT_INDENT)
            out_file.write("}\n")
        # end for sig in signature_map
        # right brace of class body
        out_file.write("}\n")
        out_file.close()
    dep_class_files = dump_dep_class_to_java(dep_class_set, interface_map)
    return (src_files, dep_class_files)


def compile_src_files(jfe_path, src_files, dep_class_files, output_file_path, keep):
    '''
    Compile java rule sources generated and archive
    '''
    tgt_dir = []
    # compile source files
    for file in src_files:
        logging.debug("Working with file : %s", file)
        root = os.path.dirname(os.path.abspath(file))
        if root not in tgt_dir:
            tgt_dir.append(root)
        logging.info("Javac compiling %s ...", file)
        subprocess.run(['javac', '-cp', '.', '-d', '.', file], check=True)

    # archive to jarfile
    jar_file = os.path.basename(output_file_path) + ".jar"
    o_file = jar_file.replace(".jar", ".o")
    jar_cmd = ['jar', '-cf', jar_file]
    for directory in tgt_dir:
        logging.debug("Visiting dir : %s", directory)
        for file in os.listdir(directory):
            src = os.path.join(directory, file)
            if os.path.isfile(src):
                ends = file.split(".")[1]
                if ends == "class":
                    jar_cmd.append(os.path.relpath(src))
    logging.info("Jar archive %s ...", jar_file)
    logging.debug("Jar archive cmd %s ...", str(jar_cmd))
    subprocess.run(jar_cmd, check=True)

    # compile jarfile
    logging.info("Xvsa compiling %s -> %s ...", jar_file, o_file)
    jfe_cmd = [jfe_path, '-RBC=true', '-cp=.', '-fC,' + jar_file, '-fB,' + o_file]
    logging.debug(jfe_cmd)
    subprocess.run(jfe_cmd, check=True)

    for file in dep_class_files:
        root = os.path.dirname(file)
        if root not in tgt_dir:
            tgt_dir.append(root)

    # ar the archive
    ar_cmd_list = ['ar', 'cr', output_file_path, o_file]
    logging.info("Archived %s", output_file_path)
    subprocess.run(ar_cmd_list, check=True)

    # clean up
    if not keep:
        shutil.rmtree("io")
        os.unlink(o_file)
        os.unlink(jar_file)
        for directory in tgt_dir:
            root = directory.split("/")[0]
            shutil.rmtree(root, True)


def read_sheet_from_file(input_file_path):
    '''
    Read intent table from excel
    '''
    xl_file = None
    try:
        xl_file = xlrd.open_workbook(filename=input_file_path)
    except:
        logging.error("Failed to open %s.", input_file_path)
        sys.exit(-1)

    # intent table sheet
    intent_sheet = None
    try:
        intent_sheet = xl_file.sheet_by_name('intent-table')
    except:
        logging.error("Failed to load intent-table sheet.")
        sys.exit(-1)
    return intent_sheet


def main():
    '''
    Main function
    '''
    parser = argparse.ArgumentParser(
        description='Convert intable table, generate rbc archive file.')
    parser.add_argument('-i', '--input', metavar='infile', required=True,
                        help='the input file name of the intent table file')
    parser.add_argument('-o', '--output', metavar='outfile',
                        help='the output file name of the library file')
    parser.add_argument('-v', '--version', action='version',
                        version='v0.1', help='show version')
    parser.add_argument('-d', '--debug', action='store_true', default=False,
                        help='debug mode, keep intermediate file and logs (default: %(default)s)')
    parser.add_argument('-k', '--keep', action='store_true', default=False,
                        help='keep intermediate files (default: %(default)s)')
    parser.add_argument('--jfe_path', help=argparse.SUPPRESS)
    parser.add_argument('--rbc_engine_path', help=argparse.SUPPRESS)
    parser.add_argument('--xvsa_path', help='specific the path of xvsa')
    args = parser.parse_args()
    input_file_path = os.path.abspath(args.input)
    logger = logging.getLogger()
    if args.debug:
        logger.setLevel(logging.DEBUG)
    else:
        logger.setLevel(logging.INFO)
    if not args.input.endswith('.xlsx'):
        logging.error("Input file should be the format of xlsx.")
        sys.exit(-1)
    if not os.path.exists(input_file_path):
        logging.error(
            'Input excel file not exist, please check, file path: %s', input_file_path)
        logging.error(
            'Please place the file in the same directory with this script.')
        sys.exit(-1)
    input_file_name = os.path.basename(input_file_path)
    output_file_path = args.output
    if output_file_path is None:
        output_file_path = os.path.join(os.path.dirname(
            input_file_path), 'lib%s.a' % os.path.splitext(input_file_name)[0])
    intent_sheet = read_sheet_from_file(input_file_path)
    xvsa_path = args.xvsa_path
    jfe_path = args.jfe_path
    rbc_engine_path = args.rbc_engine_path
    if xvsa_path is None:
          which_xvsa = subprocess.run(
              ['which', 'xvsa'], stdout=subprocess.PIPE, universal_newlines=True, check=False)
          if which_xvsa.returncode != 0:
              if jfe_path is None:
                  logging.error("Can't find xvsa, please check.")
                  sys.exit(-1)
          xvsa_path = str(which_xvsa.stdout).strip()
    if jfe_path is None:
        jfe_path = os.path.realpath(os.path.join(xvsa_path, '../../lib/1.0/mapfej'))
    if rbc_engine_path is None:
        rbc_engine_path = os.path.realpath(os.path.join(xvsa_path, '../../include/RBC_ENGINE.java'))
    compile_rbc_engine(rbc_engine_path)
    (src_files, dep_class_files) = convert_intent_table_to_java(intent_sheet)
    compile_src_files(jfe_path, src_files, dep_class_files, output_file_path, args.keep)


if __name__ == "__main__":
    main()
