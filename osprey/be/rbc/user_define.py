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

import json
import sys
import os
import subprocess

tag_dic = {}


# read required item
def get_json_item(fsm, key):
    value = fsm.get(key)
    if value is None:
        print("Missing \"" + key + "\" information")
        exit(0)
    return value


def print_indent(out_file, language):
    if language == "java":
        out_file.write("    ")
    elif language == "c/c++":
        out_file.write("  ")
    return


def process_builtin_rules(out_file, language, builtin_rules):
    if out_file is None or builtin_rules is None:
        return
    if language != "c/c++":
        print("Support C/C++ builtin rules only for now")
        return
    out_file.write("void __RBC_GLOBAL__()")
    out_file.write("\n{\n")
    for item in builtin_rules:
        condition = item.get("condition")
        if condition is None:
            out_file.write("  rbc.Rbc_enable_builtin(\"")
            out_file.write(item.get("name"))
            out_file.write("\");\n")
        else:
            out_file.write("  rbc.For_all_exec_path(")
            out_file.write(condition)
            out_file.write(", \"")
            out_file.write(item.get("name"))
            out_file.write("\");\n")
    out_file.write("}\n\n")


def process_user_apis(out_file, language, user_apis):
    # TODO
    if out_file is None or user_apis is None:
        return


# write function with rules
def write_rbc_func(out_file, language, signature, rbc_impl):
    out_file.write(signature)
    out_file.write("\n{\n")
    for line in rbc_impl:
        out_file.write("  " + line.get("value") + ";\n")
    out_file.write("}\n\n")


def process_rbc_rules(out_file, language, rbc_rules):
    if out_file is None or rbc_rules is None:
        return
    if language != "c/c++":
        print("Support C/C++ rbc rules only for now\n")
        return
    # build up tag dictionary for rbc rules
    for item in rbc_rules:
        tag = item.get("tag")
        rbc_impl = item.get("rbc_impl")
        if rbc_impl is not None:
            if tag_dic.get(tag) is not None:
                print("Duplicated tag \"" + tag + "\"")
            else:
                tag_dic[tag] = rbc_impl
    # write rules
    for item in rbc_rules:
        tag = item.get("tag")
        signature = item.get("signature")
        rbc_impl = item.get("rbc_impl")
        if rbc_impl is None:
            # bulitin tag 'malloc'
            if tag == "malloc":
                out_file.write(signature)
                out_file.write("\n{\n")
                out_file.write("  rbc.Model_decl(rbc.Declare_malloc_similar(1));\n")
                out_file.write("  return 0;\n")
                out_file.write("}\n\n")
            # builtin tag 'free'
            elif tag == "free":
                out_file.write(signature)
                out_file.write("\n{\n")
                out_file.write("  rbc.Model_decl(rbc.Declare_free_similar(1));\n")
                out_file.write("}\n\n")
            else:
                # deal with user tag binding
                rbc_impl = tag_dic.get(tag)
                if rbc_impl is None:
                    print("There's no rule implementation for \"" + tag + "\"")
                else:
                    write_rbc_func(out_file, language, signature, rbc_impl)
        else:
            write_rbc_func(out_file, language, signature, rbc_impl)


def process_fsms(out_file, language, fsms):
    if out_file is None or fsms is None:
        return
    for fsm in fsms:
        fsm_name = get_json_item(fsm, "fsm_name")
        fsm_start_state = get_json_item(fsm, "fsm_start_state")
        fsm_final_state = get_json_item(fsm, "fsm_final_state")
        fsm_transitions = get_json_item(fsm, "fsm_transitions")
        fsm_default_action = fsm.get("fsm_default_action")
        # write fsm function declare
        rbc_engine = ""
        if language == "java":
            rbc_engine = "RBC_ENGINE"
            out_file.write("  public static void " + fsm_name + "() {\n")
        elif language == "c/c++":
            rbc_engine = "rbc"
            out_file.write("void " + fsm_name + "_fsm(void)\n")
            out_file.write("{\n")
        # write fsm details
        func_model_decl = rbc_engine + ".Model_decl("
        func_build_begin = rbc_engine + ".Fsm_build_begin("
        func_start_state = rbc_engine + ".Fsm_new_start_state("
        func_final_state = rbc_engine + ".Fsm_new_final_state("
        func_add_transition = rbc_engine + ".Fsm_add_transition("
        func_default_action = rbc_engine + ".Fsm_set_default_action("
        func_build_end = rbc_engine + ".Fsm_build_end("

        print_indent(out_file, language)
        out_file.write(func_model_decl + func_build_begin + "\"" + fsm_name + "\"));\n")

        print_indent(out_file, language)
        out_file.write(func_model_decl + func_start_state + "\"" + fsm_start_state + "\"));\n")

        print_indent(out_file, language)
        out_file.write(func_model_decl + func_final_state + "\"" + fsm_final_state + "\"));\n")

        # build up tag dictionary for fsm transition
        for item in fsm_transitions:
            tag = item.get("tag")
            transit = item.get("transit")
            if transit is not None:
                if tag_dic.get(tag) is not None:
                    print("Duplicated tag \"" + tag + "\"")
                else:
                    tag_dic[tag] = transit
        # write fsm transitions
        for item in fsm_transitions:
            tag = item.get("tag")
            transit = item.get("transit")
            if transit is not None:
                # write transition
                in_state = transit.get("in_state")
                action = transit.get("action")
                demangle = subprocess.Popen(["c++filt", action], stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
                fun_name = demangle.stdout.read()
                key = transit.get("key")
                if key == "":
                    if language == "java":
                        key = "null"
                    elif language == "c/c++":
                        key = "NULL"
                condition = transit.get("condition")
                out_state = transit.get("out_state")
                error_code = transit.get("error_code")
                if error_code == "":
                    if language == "java":
                        error_code = "\"\""
                    elif language == "c/c++":
                        error_code = "0"
                else:
                    error_code = "\"" + error_code + "\""
                if language == "java":
                    out_file.write("    // " + fun_name.decode())
                    out_file.write("    ")
                    out_file.write(func_model_decl + "\n")
                    out_file.write("      " + func_add_transition + "\n")
                    out_file.write("        \"" + in_state + "\", \"" + action + "\", " + key + ",\n")
                    out_file.write("        " + condition + ", \"" + out_state + "\", " + error_code + "\n")
                    out_file.write("        )\n")
                    out_file.write("      );\n")
                elif language == "c/c++":
                    out_file.write("  ")
                    out_file.write(func_model_decl + func_add_transition + "\"" + in_state + "\", ")
                    out_file.write("\"" + action + "\", ")
                    out_file.write(key + ",\n")
                    out_file.write("                                        ")
                    out_file.write(condition + ",\n")
                    out_file.write("                                        ")
                    out_file.write("\"" + out_state + "\", ")
                    out_file.write(error_code + "));\n\n")
            else:
                transit = tag_dic.get(tag)
                if transit is None:
                    print("There's no transition implementation for \"" + tag + "\"")
                else:
                    # TODO write transition for user tag binding
                    return

        if fsm_default_action is not None:
            for action in fsm_default_action:
                print_indent(out_file, language)
                out_file.write(func_model_decl + func_default_action + "\"" + action.get("name"))
                error_code = action.get("error_code")
                if error_code == "":
                    if language == "java":
                        error_code = "\"\""
                    elif language == "c/c++":
                        error_code = "0"
                else:
                    error_code = "\"" + error_code + "\""
                out_file.write("\", " + error_code + "));\n")

        print_indent(out_file, language)
        out_file.write(func_model_decl + func_build_end + "\"" + fsm_name + "\"));\n")
        if language == "java":
            out_file.write("  }\n")
        elif language == "c/c++":
            out_file.write("}\n\n")


# main starts here
arg_size = len(sys.argv)
if arg_size < 2 or arg_size > 3:
    # print("Usage: " + sys.argv[0] + " file [-keep]\n")
    exit(0)

user_file = None
try:
    user_file = open(sys.argv[1], "r", encoding="utf-8")
except IOError as e:
    print(e)
    exit(0)

rules = json.load(user_file)

# source language type
language = get_json_item(rules, "source_language")
language = language.lower()

# package
# package = get_json_item(rules, "package")

# target_name
target_name = get_json_item(rules, "target_name")

# path information
# abs_target_path = get_json_item(rules, "abs_target_path")
# tool_chain_path = get_json_item(rules, "tool_chain_path")

# user select builtin checks
builtin_rules = rules.get("builtin_rules")
# user define APIs
user_apis = rules.get("user_apis")
# rbc_rules
rbc_rules = rules.get("rbc_rules")
# fsms
fsms = rules.get("fsms")

out_file_name = ""
if language == "java":
    out_file_name = target_name.upper() + ".java"
elif language == "c/c++":
    out_file_name = target_name + ".cxx"

out_file = None
try:
    out_file = open(out_file_name, "w")
except IOError as e:
    print(e)
    exit(0)

# write target file headers
if language == "java":
    out_file.write("package " + package + ";\n\n")
    out_file.write("import io.xc5.RBC_ENGINE;\n\n")
    out_file.write("public class " + out_file_name.split(".")[0] + " {\n")
elif language == "c/c++":
    headers = [
        "#include <stdio.h>\n",
        "#include <signal.h>\n",
        "#include <wchar.h>\n",
        "#include <sys/types.h>\n",
        "#include <sys/stat.h>\n",
        "#include <fcntl.h>\n",
        "#include \"rbc_base.h\"\n\n",
        "#ifdef __cplusplus\n",
        "extern \"C\" {\n",
        "#endif\n\n",
        "RBC_ENGINE rbc;\n",
    ]
    out_file.writelines(headers)
out_file.write("\n")

# bodies
process_builtin_rules(out_file, language, builtin_rules)
process_user_apis(out_file, language, user_apis)
process_rbc_rules(out_file, language, rbc_rules)
process_fsms(out_file, language, fsms)

# ends
if language == "java":
    out_file.write("}\n")
elif language == "c/c++":
    out_file.write("#ifdef __cplusplus\n")
    out_file.write("} // extern C\n")
    out_file.write("#endif\n")

# done
user_file.close()
out_file.close()

# compile & link
if language == "java":
    out_lib = "lib" + target_name + ".udr"
    obj_file = target_name + ".o"
    class_file = target_name.upper() + ".class"

    command_line = "javac -d . " + tool_chain_path + "/include/RBC_ENGINE.java"
    os.system(command_line)
    command_line = "javac -d . " + out_file_name
    os.system(command_line)
    source = ""
    for ss in package.split("."):
        source = source + ss + "/"
    source = source + class_file
    command_line = "xvsa -c -Wf,-RBC=true -INLINE:none " + source + " -o " + obj_file
    os.system(command_line)
    os.system("rm -rf sootOutput io " + package.split(".")[0])

    command_line = "ar cr " + out_lib + " " + obj_file
    os.system(command_line)
    os.unlink(obj_file)
elif language == "c/c++":
    out_lib32 = "lib" + target_name + "32.udr"
    out_lib64 = "lib" + target_name + "64.udr"
    obj_file32 = target_name + "32.o"
    obj_file64 = target_name + "64.o"

    command_line = "xvsa -c -m32 " + out_file_name + " -o " + obj_file32
    os.system(command_line)
    command_line = "xvsa -c -m64 " + out_file_name + " -o " + obj_file64
    os.system(command_line)

    command_line = "ar cr " + out_lib32 + " " + obj_file32
    os.system(command_line)
    os.unlink(obj_file32)
    command_line = "ar cr " + out_lib64 + " " + obj_file64
    os.system(command_line)
    os.unlink(obj_file64)

# clean up
if arg_size != 3 or sys.argv[2] != "-keep":
    os.unlink(out_file_name)
