//
// Created by xc5 on 31/7/2018.
//

#ifndef OSPREY_JGEN_EXCEPTION_H
#define OSPREY_JGEN_EXCEPTION_H

#include <string>
#include <vector>
#include <iostream>
#include <exception>

class jgen_exception : std::exception{
 public:
    std::string msg;

    jgen_exception(){
        msg = "";
        std::cerr << "Exception Created" << std::endl;
    }

    jgen_exception(char * str){
      if(str != nullptr) {
        msg = str;
        std::cerr << str << std::endl;
      }
    }

    jgen_exception(const char * str){
      if(str != nullptr) {
        msg = str;
        std::cerr << str << std::endl;
      }
    }

    jgen_exception(std::string s){
        msg = s;
        std::cerr << s << std::endl;
    }

    /*jgen_exception(jgen_exception & rhs){
        msg = rhs.msg;
    }
*/
    const char * what(){
      return msg.c_str();
    }
};

#endif //OSPREY_JGEN_EXCEPTION_H
