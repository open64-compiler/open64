/*

  Copyright (C) 2006-2009 Tsinghua University.  All Rights Reserved.

  This program is free software; you can redistribute it and/or modify it
  under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it would be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  

  Further, this software is distributed without any warranty that it is
  free of the rightful claim of any third person regarding infringement 
  or the like.  Any license provided herein, whether implied or 
  otherwise, applies only to this software file.  Patent licenses, if 
  any, provided herein do not apply to combinations of this program with 
  other software, or any other product whatsoever.  

  You should have received a copy of the GNU General Public License along
  with this program; if not, write the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston MA 02111-1307, USA.

*/

#include <cstdio>
#include <string>
#include <cctype>
#include <vector>
#include <set>
using namespace std;

void copy(const char* fni, const char* fno)
{
	FILE* fpi=fopen(fni,"r");
	FILE* fpo=fopen(fno,"w");
	if(!fpi){
		printf("Can't open input file: %s\n",fni);
		return;
	}
	if(!fpo){
		printf("Can't open output file: %s\n",fno);
		return;
	}
	char ch=fgetc(fpi);
	while(!feof(fpi)){
		fputc(ch,fpo);
		ch=fgetc(fpi);
	}

	fclose(fpi);
	fclose(fpo);
}

void append(const char* fni, const char* fno)
{
        FILE* fpi=fopen(fni,"r");
        FILE* fpo=fopen(fno,"a");
        if(!fpi){
                printf("Can't open input file: %s\n",fni);
                return;
        }
        if(!fpo){
                printf("Can't open output file: %s\n",fno);
                return;
        }
        char ch=fgetc(fpi);
        while(!feof(fpi)){
                fputc(ch,fpo);
                ch=fgetc(fpi);
        }

        fclose(fpi);
        fclose(fpo);

}

void ReadToEnd(const char* fn, string& content){
	FILE* fp=fopen(fn,"r");
	if(!fp){
		printf("Open input file failed: %s\n", fn);
		return;
	}

	char ch=fgetc(fp);
	while(!feof(fp)){
		content+=ch;
		ch=fgetc(fp);
	}

	fclose(fp);
}

void WriteFile(const char* fn, const string& content)
{
	FILE* fp=fopen(fn,"w");

	fprintf(fp, "%s", content.c_str());

	fclose(fp);
}


FILE* log_file;
#define log_file(FMT, ...) { \
	fprintf(log_file, FMT, ##__VA_ARGS__); \
}

#define log_printf(FMT, ...) { \
	fprintf(log_file, FMT, ##__VA_ARGS__); \
	printf(FMT, ##__VA_ARGS__); \
}


string trim(const string& s){
	size_t i,j;
	i=s.find_first_not_of(" \t");
	if(i==string::npos){
		return "";
	}
	j=s.find_last_not_of(" \t");
	return s.substr(i,j-i+1);
}


void split(string s, vector<string>& v, string lim){
	size_t i=0, j;
	do{
		j=s.find(lim, i);
		v.push_back(s.substr(i,j-i));
		i=j+lim.length();
	}while(j<string::npos);
}

void splitBetween(string s, vector<string>& v, string begin, string end){
	size_t i=0, j;
	do{
		j=s.find(begin, i);
		v.push_back(s.substr(i,j-i));
		if(j==string::npos)
			break;
		i=j+begin.length();
		j=s.find(end, i);
		v.push_back(s.substr(i,j-i));
		i=j+end.length();
	}while(j<string::npos);
}

void Replace(string& s, string _old, string _new){
	int i;
	vector<string> v;
	split(s, v, _old.c_str());
	s=v[0];
	for(i=1;i<v.size();i++)
		s+= _new + v[i];
}

void eraseBetween(string& s, string begin, string end){
	int i;
	vector<string> v;
	splitBetween(s, v, begin, end);
	s="";
	for(i=0;i<v.size();i+=2)
		s+=v[i];
}

class InstructionSet{
	set<string> ins;
public:
	InstructionSet(){
	}
	void read_ins_set(){
		char s[50];
		FILE* fp=fopen("inslist.txt","r");
		if(!fp){
			log_printf("Open instruction set file failed.\n");
			return;
		}
		while(fscanf(fp,"%s",s) != EOF){
			if(isalpha(s[0])){
				log_file("instruction: %s\n",s);
				ins.insert(s);
			}
		}
	}
	bool hasIns(const string& s)const{
		if(s=="") return true;
		return ins.find(s) != ins.end();
	}
	void insert(string s){
		ins.insert(s);
	}
	set<string>& get_ins_set(){
		return ins;
	}
};

InstructionSet inslist;

void SplitRules(const string& content, vector<string>& v){
	int i=0,j,s=0, d=0;
	while(1){
		j=content.find_first_of("{;}",i);
		if(j==string::npos)
			return;
		i = j+1;
		if(content[j]=='{'){
			d++;
		}
		else if(content[j]=='}'){
			d--;
		}
		else{
			if(d==0){
				v.push_back(content.substr(s,j-s));
				s = j+1;
			}
		}
	}
}
void ParseForRules(const string& content, vector<string>& rules, string& begin, string& end)
{
	vector<string> v;
	split(content, v, "%%");

	begin = v[0];
	end = v[2];

	SplitRules(v[1], rules);
}

bool ParseRule(string rule)
{
	int i,j;
	string s;
	vector<string> v;

	i=rule.find("#ins");
	if(i==string::npos)
		return true;

	i+=4;
	j=rule.find("\n",i);
	s=rule.substr(i,j-i);

	split(s,v," ");
	for(i=0;i<v.size();i++){
		if(!inslist.hasIns(trim(v[i]))){
			log_printf("rule not supported by this architecture: lacking instruction \"%s\"\n%s\n",v[i].c_str(), rule.c_str());
			return false;
		}
	}
	return true;
}

void ParseFile(const char* fn)
{
	int i;
	string content,begin,end;
	ReadToEnd(fn, content);
	eraseBetween(content, "/*", "*/");

	vector<string> rules;
	ParseForRules(content, rules, begin, end);

	begin += "%%";
	for(i=0;i<rules.size();i++){
		if(ParseRule(rules[i]))
			begin += rules[i] + ";";
	}
	begin += "%%" + end;

	set<string> ins=inslist.get_ins_set();
	for(set<string>::iterator it = ins.begin(); it != ins.end(); it++){
		log_file("used instruction: %s\n", it->c_str());
	}

	content=begin;
	Replace(content, "NOOLIVE", 
		"aa=1;$0->result = Expand_Expr($0->wn, $0->parent, $0->result);return;");
	Replace(content, "NOolive", "");
	WriteFile(fn, content);
}

int main(int argc, char* argv[])
{
	log_file=fopen("a.log","w");
	int i;
	for(i=0;i<argc;i++)
		fprintf(log_file,"%s\n",argv[i]);

	copy(argv[2], argv[1]);
	for(i=3;i<argc;i++)
		append(argv[i], argv[1]);

	inslist.read_ins_set();
	ParseFile(argv[1]);

	fclose(log_file);
	return 0;
}

