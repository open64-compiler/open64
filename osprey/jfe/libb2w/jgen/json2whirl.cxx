/**


   Version
   V0.0.0.1

   Author:
   Jason Lu @ 2018 Jul. 1 @ lu.gt@163.com

   Description:
   Using as a main entry for json2whirl
   
   Usage:
   1. Create an instance of JGEN class, JGEN(infile,outfile)
   2. JGEN.startProcess();
   3. It's done!

   Changelog:
   V0.0.0.1 Added basic structures.

*/

#include "json2whirl.h"
#include <cstdio>
#include <stdlib.h>
#include "jgen_node.h"
#include "jgen_dst.h"
#include "json_reader.h"
#include "jgen_global.h"
#include "jgen_visitor.h"

#include <iostream>
#include <string>

/***
    JGEN as J2WGEN or Json2Whirl 
 */

namespace JGEN
{
using std::cout;
using std::endl;

class Json2Whirl {
 private:

  int stage;
  int stage_json;
  int stage_parse;
  int stage_write;
  int last_error;

  std::string _in_filepath;
  std::string _out_filepath;

 public:
  /**
   *  Empty Initializer
   */
  Json2Whirl () : stage (0), stage_json (0),
                  stage_write (0), stage_parse (0), last_error (0)
  {
    _in_filepath = "";
    _out_filepath = "";
  }

  /**
   *  Default
   *  @param json_path Path to JSON Input File
   *  @param out_path Path to Output Whirl (.B) file
   */
  Json2Whirl (std::string &json_path,
              std::string &out_path) : stage (0), stage_json (0), stage_write (0), stage_parse (0),
                                       last_error (0)
  {
    _in_filepath = json_path;
    _out_filepath = out_path;
  }

  /**
   *  Set Path to JSON File
   */
  void setJsonPath (std::string &json_path)
  {
    _in_filepath = json_path;
  }

  void setOutPath (std::string &out_path)
  {
    _out_filepath = out_path;
  }

  int startProcess ()
  {
    stage = JGEN_STARTING;
    if (_in_filepath == "")
      {
        return JGEN_ERR_NO_INPUT_FILEPATH;
      }
    if (_out_filepath == "")
      {
        return JGEN_ERR_NO_OUTPUT_FILEPATH;
      }
    return proceed ();
  }

 private:
  JGEN_Root root;
  Json_IR ir;
  // Proceed to next stage in JGEN
  int proceed ()
  {
    /*
       Continuously Do Stuff
     */
    int outcome = 0;
    while (stage != JGEN_FATAL_STOPPED && stage != JGEN_SUCCESS)
      {
        if (stage <= JGEN_JSON)
          {
            ir.open (_in_filepath.c_str ());
            int y = 0;
            if((y =ir.read()) < 0){
              logger("-- [FATAL ERROR]: cannot read from json." + int2str(y));
              stage = JGEN_FATAL_STOPPED;
              continue;
            }
            if(ir.isNull()) {
              logger("-- [FATAL ERROR]: cannot get a valid json");
              stage = JGEN_FATAL_STOPPED;
            } else {
              stage = JGEN_PARSE;
            }
          }
        else if (stage <= JGEN_PARSE)
          {
            root.init (_out_filepath);
            JGEN_IR_Decl * top_decl = ir.get_top_decl ();
            root.traverse_decl (top_decl);
            //void * parse_out = parse.parse(jsonobj);
            stage = JGEN_WRITE;
          }
        else if (stage <= JGEN_WRITE)
          {
            root.finish ();
            stage = JGEN_SUCCESS;
          }
      }

    if (stage == JGEN_FATAL_STOPPED)
      {
        return -1;
      }

    // Success !
    return 0;

  }

  void parse_stage_outcome (int out)
  {
    int remainder = 0;
    if (out <= 10000 || out >= 99999)
      {
        stage = JGEN_FATAL_STOPPED;
        last_error = out;
        return;
      }
    remainder = out % 10000;
    if (remainder == 9999)
      {
        // FATAL;
        stage = JGEN_FATAL_STOPPED;
      }
    else if (remainder == 8000)
      {
        stage = JGEN_SUCCESS;
      }
  }
};

void output_help ()
{
  std::cout << "-------------------------------------------" << std::endl;
  std::cout << "       Jgen : Json to Whirl Generator      \n" <<
            " Further description please go to wiki/docs  " << std::endl;
  std::cout << " -  https://github.com/lugt/opencc_cmake   -" << std::endl;
  std::cout << " -  Base:2018.08.1   WH:5.0     JS:18.07   -" << std::endl;
  std::cout << "--------------------------------------------" << std::endl;
  std::cout << "    Author : Jason Lu    -  lu.gt@163.com   " << std::endl;
  std::cout << "    Author : Shijie Li   - shijieli@qq.com  " << std::endl;
  std::cout << "--------------------------------------------" << std::endl;
  std::cout << "Usage  : jgen <INPUT_JSON_FILE> <OUTPUT_B_FILE>" << std::endl;
  std::cout << "Params : jgen -fS,<INPUT_JSON_FILE> -fB,<OUTPUT_B_FILE> [... PARAMS]" << std::endl;
  std::cout << "Help   : jgen --help " << std::endl;
  std::cout << "--------------------------------------------" << std::endl;
}

bool isParamParseState = false;
unsigned int addr_count = 0;
Json2Whirl *g = nullptr;

void jgen_param_parse (string param);
int jgen_process_command (int argc, char **argv)
{

  if (argc < 2)
    {
      output_help ();
      return -1;
    }

  std::string fni = argv[1], in_file = "", out_file = "";
  for (int i = 1; i < argc; i++)
    {
      fni = argv[i];
      if (fni == "-h" || fni == "--help" || fni == "h" || fni == "help" || fni == "-v" || fni == "--version"
          || fni == "--v" || fni == "--h" || fni == "-version")
        {
          output_help ();
          return -2;
        }
      if (fni[0] == '-' || isParamParseState)
        {
          jgen_param_parse (fni);
        }
      else
        {
          if (addr_count == 0)
            {
              in_file = fni;
            }
          else if (addr_count == 1)
            {
              out_file = fni;
            }
          addr_count++;
        }

    }

  std::cout << "-- Reading from " << in_file << endl;
  if (in_file.size () <= 0)
  {
    logger("-- [Json2Whirl::processCommand] please specify input json file location.");
    return -2;
  }
  if (out_file.empty())
  {
      out_file = "out.B";
  }
  std::cout << "-- Output to " << out_file << std::endl;
  // All Main Process
  g = new Json2Whirl (in_file, out_file);
  return 0;
}

void jgen_param_parse (string param)
{
  if (param.size () <= 0)
    {
      return;
    }
  if (param == "-s")
    {
      // silent
    }
  else if (param == "-vb")
    {
      // verbose
    }
  else if (param == "-no-class")
    {
      // Do not parse/create Class
    }
  else if (param == "-no-method")
    {
      // Do not parse/create Method
    }
  else if (param == "-ro")
    {
      // (Read Only) Just read JSON, don't write to .B
    }
}


void jgen_act_accord ()
{
  if (g == nullptr)
    {
      throw std::exception ();
    }
  g->startProcess ();
}

}

int main (int argc, char **argv)
{
  if (JGEN::jgen_process_command (argc, argv) < 0)
    {
      return -10;
    }
  JGEN::jgen_act_accord ();
}
