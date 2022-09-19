/*
 * Copyright 2003, 2004 PathScale, Inc.  All Rights Reserved.
 */

/*

  Copyright (C) 2000, 2001 Silicon Graphics, Inc.  All Rights Reserved.

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

  Contact information:  Silicon Graphics, Inc., 1600 Amphitheatre Pky,
  Mountain View, CA 94043, or:

  http://www.sgi.com

  For further information regarding this notice, see:

  http://oss.sgi.com/projects/GenInfo/NoticeExplan

*/


//   si_gen
/////////////////////////////////////
//
//  Description:
//
//      Digest the description of a particular hardware implementation's
//      scheduling information and generate a c file that describes the
//      features.  The interface is extensively described in si_gen.h.
//
/////////////////////////////////////

//  $Revision: 1.1.1.1 $
//  $Date: 2005/10/21 19:00:00 $
//  $Author: marcel $
//  $Source: /proj/osprey/CVS/open64/osprey1.0/common/targ_info/generate/si_gen.cxx,v $


#include <assert.h>
#include <stdio.h>
#include <unistd.h>
#include <limits.h>
#include <stdarg.h>
#include <list>
#include <map>
#include <vector>

#include "topcode.h"
#include "targ_isa_properties.h"
#include "targ_isa_subset.h"
#include "targ_isa_operands.h"

#include "si_gen.h"

static ISA_SUBSET machine_isa;

// Parameters:
const int bits_per_long = 32;
const int bits_per_long_long = 64;
const bool use_long_longs = true;       // For now always
const int max_operands = ISA_OPERAND_max_operands;
const int max_results = ISA_OPERAND_max_results;

/////////////////////////////////////
int Mod( int i, int j )
/////////////////////////////////////
//  Mathematically correct integer modulus function.  Unlike C's
//  builtin remainder function, this correctly handles the case where
//  one of the two arguments is negative.
/////////////////////////////////////
{
  int rem;

  if ( j == 0 )
    return i;

  rem = i % j;

  if ( rem == 0 )
    return 0;

  if ( (i < 0) != (j < 0) )
    return j + rem;
  else
    return rem;
}

/////////////////////////////////////
static void Maybe_Print_Comma(FILE* fd, bool& is_first)
/////////////////////////////////////
// Print a "," to <fd> if <is_first> is false.  Update <is_first> to false.
// Great for printing C initializers.
/////////////////////////////////////
{
  if ( is_first )
    is_first = false;
  else
    fprintf(fd,",");
}

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
class GNAME {
/////////////////////////////////////
// A generated name for a variable in the generated file.  This supports a
// unified method for naming and getting the names of the objects we generate.
/////////////////////////////////////
public:
  GNAME();
  // Generate a unique name.  Don't care about prefix.
  GNAME(char* prefix);
  // Generate a unique name.  Force a particular prefix.
  GNAME(GNAME& other);
  // Generate a name that is a copy of <other>.  The name will not be unique.
  // Really only useful when <other> is about to be destructed, but we still
  // need to refer to it.
  char* Gname();
  // Return the name.  This is the name under which the object is defined.
  char* Addr_Of_Gname();
  // Return a pointer to the named object.
  void Stub_Out();
  // We've decided not to define the object after all but we may still want a
  // pointer to it.  After this call, Addr_Of_Gname will return 0.

private:
  char gname[16];       // Where to keep the name.  (This could be more
                        //   hi-tech, but why?
  bool stubbed;         // Stubbed-out?
  static int count;     // For generating the unique names.
};

int GNAME::count = 0;

GNAME::GNAME() : stubbed(false) {
  sprintf(gname,"&gname%d",count++);
}

GNAME::GNAME(char* prefix) : stubbed(false) {
  assert(strlen(prefix) <= 8);
  sprintf(gname,"&%s%d",prefix,count++);
}

GNAME::GNAME(GNAME& other) : stubbed(false) {
  sprintf(gname,"%s",other.gname);
}

char* GNAME::Gname() {
  if (stubbed)
    return "0";
  else
    return gname + 1;
}

char* GNAME::Addr_Of_Gname() {
  if (stubbed)
    return "0";
  else
    return gname;
}

void GNAME::Stub_Out() {
  stubbed = true;
}

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
class RES_WORD {
/////////////////////////////////////
// A machine word in the resource reservation table.  We use an encoding of
// resources that allows very effecient checking for resources with more than
// a single instance.  Shifted arithmetic is used to check for resource
// availability and to reserve resources.  Each resource has a reserved field
// in the RES_WORD.  The field for a given resource r is log2(count(r)) + 1
// bits wide.  This field is wide enough to hold the count of members of r and
// one extra bit to the left of the count.  This bit is called the "overuse
// bit".  The field is initialized to be 2**field_width - (count + 1).  This
// means that we can add count elements to the field without effecting the
// overuse bit, but adding more elements to the field will set the overuse
// bit.  So we can can check for resource availability of all the resources
// reqpresented in a word with an an add (of the counts required) and a mask
// of the overuse bits.  If the result is non-zero, there is a resource
// overuse (conflict).
//
// In theory this could be any natural sized integer supported on the host
// architecture, but for now we will always use long longs so we don't have to
// worry about generating/checking more than one word/cycle.  This could be
// extended, but it would mean moving the actual resource checking to the
// generated side of the compile time interface (since it would need to know
// the format of the resource reservation table which could change...

public:
  static void Find_Word_Allocate_Field(int width, int count,
                                       int &word, int &bit);
  // Allocate the first available resource field with <widtn> bits to hold
  // <count> resources.  A new resource word is allocated if required.  On
  // return, <word> and <bit> hold the word index and bit index of the
  // allocated word.

  static void Output_All(FILE* fd);
  // Write resource word descriptions to output.

private:
  int bit_inx;                          // Index of first next free bit
  const int word_inx;                   // My index in table
  long long initializer;                // Value when no resources used
  long long overuse_mask;               // Bits to check
                                        //   for overuse after adding new
                                        //   resources
  static std::list<RES_WORD*> res_words;     // Of all res_words in order
  static int count;                     // Of all resource words
  static bool has_long_long_word;       // Will we need to use long longs for
                                        //   resource words?

  RES_WORD()
    : bit_inx(0),
      word_inx(count++),
      initializer(0),
      overuse_mask(0)
  {
    res_words.push_back(this);
  }
  bool Allocate_Field(int width, int count, int &word, int &bit);
};

std::list<RES_WORD*> RES_WORD::res_words;
int RES_WORD::count = 0;
bool RES_WORD::has_long_long_word = false;

/////////////////////////////////////
bool RES_WORD::Allocate_Field(int width, int count, int &word, int &bit)
/////////////////////////////////////
// Allocate a field <width> bits wide to hold <count> elements.  Return true
// to indicate success with <word> set to my word_inx and <bit> set to the
// the bit index of the start of the field.
/////////////////////////////////////
{
  int new_inx = bit_inx + width;

  if (    use_long_longs && new_inx >= bits_per_long_long
       || !use_long_longs && new_inx >= bits_per_long
  ) {
    return false;
  }

  if ( new_inx >= bits_per_long )
    has_long_long_word = true;

  word = word_inx;
  bit = bit_inx;
  initializer |= ((1ll << (width - 1)) - (count + 1)) << bit_inx;
  overuse_mask |= (1ll << (width - 1)) << bit_inx;
  bit_inx += width;
  return true;
}

void RES_WORD::Find_Word_Allocate_Field(int width, int count,
                                        int &word, int &bit)
{
  std::list<RES_WORD*>::iterator rwi;
  for ( rwi = res_words.begin(); rwi != res_words.end(); ++rwi ) {
    if ( (*rwi)->Allocate_Field(width,count,word,bit) )
      return;
  }

  RES_WORD* new_res_word = new RES_WORD();

  if ( ! new_res_word->Allocate_Field(width,count,word,bit) ) {
    fprintf(stderr,"### Cannot allocate field for %d resources\n",count);
    exit(EXIT_FAILURE);
  }
}

void RES_WORD::Output_All(FILE* fd)
{
  if ( count == 0 )
    fprintf(stderr,"ERROR: no resource words allocated.\n");
  else if ( count > 1 ) {
    fprintf(stderr,"ERROR: cannot handle %d > 1 long long worth of "
                   "resource info.\n",
                   count);
  }
  else {
    // Important special case.  We don't need a vector of resource words at all
    // and can just use a scalar.
    fprintf(fd,"const SI_RRW SI_RRW_initializer = 0x%llx;\n",
               res_words.front()->initializer);
    fprintf(fd,"const SI_RRW SI_RRW_overuse_mask = 0x%llx;\n",
               res_words.front()->overuse_mask);
  }
}

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
class RES {
/////////////////////////////////////
// A machine level resource.
/////////////////////////////////////

public:
  RES(char *name,int count);
  // <name> is used for documentation and debugging.  <count> is the number of
  // elements in the class.

  static RES* Get(int id);
  // Find and return the resource with the given <id>.

  char* Name() const { return name; }
  // Return debugging name.

  char* Addr_Of_Gname() { return gname.Addr_Of_Gname(); }
  // Return name of pointer to this resource object (in generated code).

  unsigned int Count() const { return count; }
  // How may members?

  int Word() const { return word; }
  // Index of word in resource reservation table.  (We have sort of allowed
  // there to be more than one, but this is probably not fully working.)

  int Id() const { return id; }
  // Unique ID of resource.  Index into table of pointers to resources in the
  // generated file.

  unsigned int Shift_Count() const { return shift_count; }
  // Bit index of the field in the resource reservation word.

  static void Output_All( FILE* fd );
  // Write out all the resource info to <fd>.

private:
  const int count;          // Available per cycle
  char* const name;         // For documentation and debugging
  GNAME gname;              // Generated symbolic name
  int word;                 // Which word in the table?
  int field_width;          // How wide the field?
  int shift_count;          // How much to shift (starting pos of the low
                            //   order bit
  const int id;             // Unique numerical identifier
  static int total;         // Total number of different RESs (not the the
                            //   total of their counts, 1 for each RES)
  static std::map<int,RES*> resources;
                            // Map of all resources, ordered by their Id's
  void Calculate_Field_Width();
  void Calculate_Field_Pos();

  static void Calculate_Fields();
  // Calculate fields for all resources.  This can only be done at the very
  // end becaue we may not know for sure that there are no multiple resources
  // until then.

  void Output( FILE* fd );
};

int  RES::total = 0;
std::map<int,RES*> RES::resources;

RES::RES(char *name, int count)
// constructor maintains list of all resources.
  : count(count), name(name), id(total++), gname("resource")
{
  resources[id] = this;
}

RES* RES::Get(int i)
{
  assert(total > 0 && i >= 0 && i < total);
  return resources[i];
}

void RES::Output_All( FILE* fd )
{
  int i;

  Calculate_Fields();

  for ( i = 0; i < total; ++i )
    resources[i]->Output(fd);

  fprintf(fd,"const int SI_resource_count = %d;\n",total);
  fprintf(fd,"SI_RESOURCE * const SI_resources[] = {");

  bool is_first = true;
  for ( i = 0; i < total; ++i ) {
    Maybe_Print_Comma(fd,is_first);
    fprintf(fd,"\n  %s",resources[i]->gname.Addr_Of_Gname());
  }

  fprintf(fd,"\n};\n");
}

/////////////////////////////////////
void RES::Calculate_Field_Width()
/////////////////////////////////////
//  Calculate the number of bits for my field and set <field_width>
//  accordingly.
/////////////////////////////////////
{
  int i;

  assert(count > 0);

  for ( i = 31 ; i >= 0 ; --i ) {
    if ((( (int) 1) << i) & count) {
      field_width = i + 2;
      break;
    }
  }
}

void RES::Calculate_Field_Pos()
{
  Calculate_Field_Width();
  RES_WORD::Find_Word_Allocate_Field(field_width,count,word,shift_count);
}

/////////////////////////////////////
void RES::Calculate_Fields()
/////////////////////////////////////
//  See interface description.
//  Description
/////////////////////////////////////
{
  for ( int i = 0; i < total; ++i )
    resources[i]->Calculate_Field_Pos();
}

/////////////////////////////////////
void RES::Output( FILE* fd )
/////////////////////////////////////
// Allocate my field in the resource reservation table.
/////////////////////////////////////
{
  fprintf(fd,"SI_RESOURCE %s = {\"%s\",%d,%d,%d,%d};\n",
             gname.Gname(),
             name,
             id,
             count,
             word,
             shift_count);
}

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
class RES_REQ {
/////////////////////////////////////
// A resource requirement.  Represents all the resources needed to perform an
// instruction.
/////////////////////////////////////

public:
  RES_REQ();

  bool Add_Resource(const RES* res, int cycle);
  // Require an additional resource <res> at the given <cycle> relative to
  // my start.  Return indication of success.  If adding the resource would
  // create an overuse, don't add it and return false.

  void Output(FILE* fd);
  // Output my definition and initialization.

  char* Addr_Of_Gname() { return gname.Addr_Of_Gname(); }
  // Return name of pointer to me (in generated code).

  char* Gname() { return gname.Gname(); }
  // Return my name (in generated code).

  bool Compute_Maybe_Output_II_RES_REQ(int ii, FILE* fd,
                                       GNAME*& res_req_gname,
                                       GNAME*& resource_id_set_gname );
  // When software pipelining, we want to check all the resources for a given
  // cycle of the schedule at once.  Because the resource requirement may be
  // longer than the II into which we are trying to schedule it, we statically
  // combine the resource requirements for each II shorter than the number of
  // cycles in the request.  This function does the combining and returns a
  // bool to indicate whether the resource requirement can be scheduled in the
  // given <ii>.  If it can, the combined a definition and initialization of
  // resource requirement is output to <fd> under the GNAME <res_req_gname>.
  // A cycle indexed set of resource id's used is also output under the GNAME
  // <resource_id_set_gname>.

  int Max_Res_Cycle() { return max_res_cycle; }
  // Return the cycle (relative to my start) of the latest resource I
  // require.  (Used to know how many II relative resource requirements need
  // to be computed/output.)

  void Compute_Output_Resource_Count_Vec(FILE* fd);
  // Count up all the resources of each kind that I require (in all my cycles)
  // and output a definition and initialization.

  char* Res_Count_Vec_Gname() { return res_count_vec_gname.Gname(); }
  // Return name of pointer to start of my resource count vector.

  int Res_Count_Vec_Size() const { return res_count_vec_size; }
  // Return length of my resource count vector.

  char* Res_Id_Set_Gname() { return res_id_set_gname.Gname(); }
  // Return name of pointer to start of vector of resource id sets, one per
  // cycle.

private:

  /////////////////////////////////////
  class CYCLE_RES {
  /////////////////////////////////////
  // A cycle and resource (id) combined into a single object.  Used as a key
  // into a map so we can find out how may of the given resources are required
  // in the given cycle.
  /////////////////////////////////////

  public:
    CYCLE_RES(int cycle, const RES* res) : cycle(cycle), res_id(res->Id()) {}
    // Construct the <cycle,res> combination.

    int Cycle() const { return cycle; }
    // Return cycle component.

    RES* Res() const { return RES::Get(res_id); }
    // Return resource component.

    friend bool operator < (const CYCLE_RES a, const CYCLE_RES b)
    // Ordering for map.
    {  // I didn't want to put this inline, but mongoose C++ forced me to.
      return    a.cycle< b.cycle
             || a.cycle == b.cycle && a.res_id < b.res_id;
    }

    CYCLE_RES()
    // Horrible useless required constructor required by STL map.
     : cycle(0), res_id(0)
    {  // Also forced inline by mongoose C++.
      fprintf(stderr,"### Default initializer for CYCLE_RES"
              " shouldn't happen.\n");
    }

  private:
    const short cycle;
    const short res_id;
  };

  typedef std::map< CYCLE_RES,int,std::less <CYCLE_RES> > CYCLE_RES_COUNT_MAP;
  // For keeping track of the number of resources of a given type in a given
  // cycle.  <cycle,res> => count

  int max_res_cycle;
  // Latest cycle with a resource requirement

  CYCLE_RES_COUNT_MAP cycle_res_count;
  // <cycle,res> -> count required

  GNAME gname;
  // My symbolic name.

  GNAME res_count_vec_gname;
  // Symbolic name of my resource count vector.

  GNAME res_id_set_gname;
  // Symbolic name of vector of resource id sets

  int res_count_vec_size;
  // How big it is.

  bool Compute_II_RES_REQ(int ii, RES_REQ& ii_res_req);
};

RES_REQ::RES_REQ()
  : max_res_cycle(-1),
    gname("res_req")
{}

bool RES_REQ::Add_Resource(const RES* res, int cycle)
{
  assert(cycle >= 0);
  if ( cycle > max_res_cycle ) max_res_cycle = cycle;

  CYCLE_RES cr = CYCLE_RES(cycle,res);
  int count = cycle_res_count[cr];

  if ( count >= res->Count() )
    return false;

  cycle_res_count[cr] = ++count;
  return true;
}

/////////////////////////////////////
bool RES_REQ::Compute_II_RES_REQ(int ii, RES_REQ& ii_res_req)
/////////////////////////////////////
//  Compute my <ii> relative resourse requirement info <ii_res_req> and return
//  a bool to indicate whether it is possible to issue me in a loop with <ii>
//  cycles.
/////////////////////////////////////
{
  CYCLE_RES_COUNT_MAP::iterator mi;
  for (mi = cycle_res_count.begin(); mi != cycle_res_count.end(); ++mi) {
    int cycle = (*mi).first.Cycle();
    RES* res = (*mi).first.Res();
    int count = (*mi).second;

    for (int i = 0; i < count; ++i) {
      if ( ! ii_res_req.Add_Resource(res,Mod(cycle,ii)) )
        return false;
    }
  }

  return true;
}

bool RES_REQ::Compute_Maybe_Output_II_RES_REQ(int ii, FILE* fd,
                                              GNAME*& res_req_gname,
                                              GNAME*& res_id_set_gname_ref )
{
  RES_REQ ii_res_req;

  if ( ! Compute_II_RES_REQ(ii,ii_res_req) )
    return false;

  ii_res_req.Output(fd);
  res_req_gname = new GNAME(ii_res_req.gname);
  res_id_set_gname_ref = new GNAME(ii_res_req.res_id_set_gname);
  return true;
}

void RES_REQ::Compute_Output_Resource_Count_Vec(FILE* fd)
{
  CYCLE_RES_COUNT_MAP::iterator mi;
  std::map<int,int,std::less<int> > res_inx_count;  // res_id => count

  // Sum up the number of each required
  for (mi = cycle_res_count.begin(); mi != cycle_res_count.end(); ++mi) {
    RES* res = (*mi).first.Res();
    int count = (*mi).second;

    res_inx_count[res->Id()] += count;
  }

  res_count_vec_size = res_inx_count.size();

  if ( res_count_vec_size == 0 ) {
    res_count_vec_gname.Stub_Out();
    return;
  }

  // Print it out
  fprintf(fd,"static SI_RESOURCE_TOTAL %s[] = {",
             res_count_vec_gname.Gname());

  bool is_first = true;
  std::map<int,int,std::less<int> >::iterator mj;
  for (mj = res_inx_count.begin(); mj != res_inx_count.end(); ++mj) {
    RES* res = RES::Get((*mj).first);  // You'd think STL would allow
    int count = (*mj).second;          // something less ugly!  But no.

    Maybe_Print_Comma(fd,is_first);
    fprintf(fd,"\n  {%s,%d} /* %s */",
               RES::Get(res->Id())->Addr_Of_Gname(),count,res->Name());
  }
  fprintf(fd,"\n};\n");
}

void RES_REQ::Output(FILE* fd)
{
  int i;
  CYCLE_RES_COUNT_MAP::iterator mi;
  std::vector<unsigned long long> res_vec((size_t) max_res_cycle + 1,0);
  std::vector<unsigned long long> res_used_set((size_t) max_res_cycle + 1,0);

  for (mi = cycle_res_count.begin(); mi != cycle_res_count.end(); ++mi) {
    int cycle = (*mi).first.Cycle();  // You'd think this could be abstracted,
    RES* res = (*mi).first.Res();     // but I couldn't even explain the
    long long  count = (*mi).second;  // the concept to Alex S.

    res_vec[cycle] += count << res->Shift_Count();
    res_used_set[cycle] |= 1ll << res->Id();
  }

  fprintf(fd,"static const SI_RRW %s[] = {\n  %d",
             gname.Gname(),
             max_res_cycle + 1);

  for ( i = 0; i <= max_res_cycle; ++i )
    fprintf(fd,",\n  0x%llx",res_vec[i]);

  fprintf(fd,"\n};\n");

  if ( max_res_cycle < 0 ) {
    res_id_set_gname.Stub_Out();
    return;
  }

  fprintf(fd,"static const SI_RESOURCE_ID_SET %s[] = {",
             res_id_set_gname.Gname());

  bool is_first = true;
  for ( i = 0; i <= max_res_cycle; ++i ) {
    Maybe_Print_Comma(fd,is_first);
    fprintf(fd,"\n  0x%llx",res_used_set[i]);
  }

  fprintf(fd,"\n};\n");
}

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
class ISLOT {
/////////////////////////////////////
// An issue slot.  This is for modeling the horrible beast skewed pipe and we
// hope that it will be useful enough to support experimentation with related
// ideas.
/////////////////////////////////////

public:
  ISLOT(char* name, int skew, int avail_count);
  // <name> is for documentation and debugging.  <skew> gives a latency skew
  // instructions issued in me.

  char* Addr_Of_Gname() { return gname.Addr_Of_Gname(); }
  // Return pointer to my name in generated.

  static int Count() { return count; }
  // How may instructions can issue in me.

  static void Output_All(FILE* fd);
  // Output all the issue slots and a vector of pointers to them all.

private:
  char* const name;             // User supplied for documentation & debugging
  const int skew;               // Latency skew
  const int avail_count;        // How many instructions can happen in it
  GNAME gname;                  // Symbolic name in generated
  static std::list<ISLOT*> islots;  // All the created islot
  static int count;             // How many issue slots total?
};

std::list<ISLOT*> ISLOT::islots;
int ISLOT::count = 0;

ISLOT::ISLOT(char* name, int skew, int avail_count)
  : name(name),
    skew(skew),
    avail_count(avail_count)
{
  islots.push_back(this);
  ++count;
}

void ISLOT::Output_All(FILE* fd)
{
  std::list<ISLOT*>::iterator isi;

  fprintf(fd,"const int SI_issue_slot_count = %d;\n",count);

  for ( isi = islots.begin(); isi != islots.end(); ++isi ) {
    ISLOT* islot = *isi;
    fprintf(fd,"static SI_ISSUE_SLOT %s = { \"%s\",%d,%d};\n",
            islot->gname.Gname(),
            islot->name,
            islot->skew,
            islot->avail_count);
  }

  if ( count == 0 )
    fprintf(fd,"SI_ISSUE_SLOT * const SI_issue_slots[1] = {0};\n");
  else {
    fprintf(fd,"SI_ISSUE_SLOT * const SI_issue_slots[%d] = {",count);

    bool is_first = true;
    for ( isi = islots.begin(); isi != islots.end(); ++isi ) {
      ISLOT* islot = *isi;
      Maybe_Print_Comma(fd,is_first);
      fprintf(fd,"\n  %s",islot->Addr_Of_Gname());
    }

    fprintf(fd,"\n};\n");
  }
}

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
class LATENCY_INFO {
/////////////////////////////////////
// Describes latency information for an instruction group's operands or
// results.
/////////////////////////////////////

public:
  LATENCY_INFO(int max_elements);
  // <max_elements> is the maximum number either of operands or results.

  void Set_Any_Time(int time);
  // Any (all) of the operands or results have <time> as access or available
  // time.

  void Set_Time(int index, int time);
  // <index>'th operand or result has <time> as access or available time.

  void Output(FILE* fd);
  // Output latency vector to <fd>.

  char* Gname() { return gname.Gname(); }
  // Return name of pointer to me in generated file.

private:
  GNAME gname;                  // Name in generated
  const int max_elements;       // Maximum number of operands or results
  bool any_time_defined;        // Overriding time defined
  int any_time;                 // And here it is
  std::vector<bool> times_defined;   // Times for each operands defined?
  std::vector<int> times;            // And here they are
};

LATENCY_INFO::LATENCY_INFO(int max_elements)
  : gname("latency"),
    max_elements(max_elements),
    any_time_defined(false),
    times_defined(max_elements,false),
    times(max_elements)
{}

void LATENCY_INFO::Set_Any_Time(int time)
{
  if ( any_time_defined ) {
    fprintf(stderr,"### Warning any_time redefined for %s.  "
                   "Was %d.  Is %d\n",
                   gname.Gname(),
                   any_time,
                   time);
  }

  any_time_defined = true;
  any_time = time;
}

void LATENCY_INFO::Set_Time(int index, int time)
{
  if ( any_time_defined ) {
    fprintf(stderr,"### WARNING: %s setting specific time after any time.  "
                   "Any %d.  Specific %d\n",
                   gname.Gname(),
                   any_time,
                   time);
  }

  assert(index < max_elements);

  if ( times_defined[index] ) {
    fprintf(stderr,"### WARNING: Resetting %s time.  "
                   "Was %d. Now is %d\n",
                   gname.Gname(),
                   time,
                   times[index]);
  }

  times_defined[index] = true;
  times[index] = time;
}

void LATENCY_INFO::Output(FILE* fd)
{
  fprintf(fd,"static const mUINT8 %s[] = {",gname.Gname());

    bool is_first = true;
    std::vector<int>::iterator i;
    for ( i = times.begin(); i < times.end(); ++i ) {
      Maybe_Print_Comma(fd,is_first);
      fprintf(fd,"%d",any_time_defined ? any_time : *i);
    }

    fprintf(fd,"};\n");
}

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
class INSTRUCTION_GROUP {
/////////////////////////////////////
// Represents one of the instruction groups, a common piece of scheduling
// information for a set of instructions.
/////////////////////////////////////

public:
  // These functions correspond exactly with the defined C client interface.
  INSTRUCTION_GROUP(char* name);
  void Set_Any_Operand_Access_Time(int time);
  void Set_Operand_Access_Time(int operand_index, int time);
  void Set_Any_Result_Available_Time(int time);
  void Set_Result_Available_Time(int result_index, int time);
  void Set_Load_Access_Time( int time );
  void Set_Last_Issue_Cycle( int time );
  void Set_Store_Available_Time( int time );
  void Add_Resource_Requirement(const RES* res, int cycle);
  void Add_Valid_ISLOT(ISLOT* islot);
  void Set_Write_Write_Interlock();

  static void Output_All(FILE* fd);
  // Write them all out

  char* Addr_Of_Gname() { return gname.Addr_Of_Gname(); }
  // Name of pointer to me in generated file.

private:
  int id;                               // Index in vector of same
  GNAME gname;                          // Variable name in generated    
  char* const name;                     // User supplied name for documentation
  RES_REQ res_requirement;              // Required to issue

  std::list<ISLOT*> valid_islots;            // If there are any issue slots at all
  GNAME islot_vec_gname;                // Variable name of above in generated

  LATENCY_INFO operand_latency_info;    // When operands latch
  LATENCY_INFO result_latency_info;     // When results available

  int load_access_time;                 // When loads access memory
  int last_issue_cycle;			// Last issue cycle in simulated insts
  int store_available_time;             // When stores make value available in
                                        //   memory 

  bool write_write_interlock;           // For simulator

  GNAME ii_res_req_gname;
  // Generated name of vector of resource requirements for each II less than
  // the total number of cycles in res_requirement (one based).

  GNAME ii_res_id_set_gname;
  // Generate name of vector of resource id sets for each II less than
  // the total number of cycles in res_requirement (one based).

  unsigned long long bad_iis[2];
  // Tells whether it is possible to schedule at all at a given II.  This
  // could be a more flexible data structure.  As it is, it is limited to 128
  // bad IIs, but I think this will be enough.

  static std::list<INSTRUCTION_GROUP*> instruction_groups;
  // All the defined instruction groups.

  static int count;
  // Of all instruction groups.  Used to generate ids.

  int II_Info_Size() { return res_requirement.Max_Res_Cycle(); }
  // Latest cycle in which I need a resource

  void Output_II_Info(FILE* fd);
  void Output_Latency_Info(FILE* fd);
  void Output_Issue_Slot_Info(FILE* fd);
  void Output(FILE* fd);
};


std::list<INSTRUCTION_GROUP*> INSTRUCTION_GROUP::instruction_groups;
int INSTRUCTION_GROUP::count = 0;

INSTRUCTION_GROUP::INSTRUCTION_GROUP(char* name)
    : id(count++),
      name(name),
      operand_latency_info(max_operands),
      result_latency_info(max_results),
      load_access_time(0),
      last_issue_cycle(0),
      store_available_time(0),
      write_write_interlock(false),
      ii_res_req_gname("ii_rr")
{
  bad_iis[0] = 0;
  bad_iis[1] = 0;
  instruction_groups.push_back(this);
}

void INSTRUCTION_GROUP::Set_Any_Operand_Access_Time(int time)
{
  operand_latency_info.Set_Any_Time(time);
}

void INSTRUCTION_GROUP::Set_Operand_Access_Time(int operand_index, int time)
{
  operand_latency_info.Set_Time(operand_index,time);
}

void INSTRUCTION_GROUP::Set_Any_Result_Available_Time(int time)
{
  result_latency_info.Set_Any_Time(time);
}

void INSTRUCTION_GROUP::Set_Result_Available_Time(int result_index, int time)
{
  result_latency_info.Set_Time(result_index,time);
}

void INSTRUCTION_GROUP::Set_Load_Access_Time( int time )
{
  load_access_time = time;
}

void INSTRUCTION_GROUP::Set_Last_Issue_Cycle( int time )
{
  last_issue_cycle = time;
}

void INSTRUCTION_GROUP::Set_Store_Available_Time( int time )
{
  store_available_time = time;
}

void INSTRUCTION_GROUP::Add_Resource_Requirement(const RES* res, int cycle)
{
  if (! res_requirement.Add_Resource(res,cycle)) {
    fprintf(stderr,"### ERROR: Impossible resource request for "
                    "instruction group %s.\n",
                    name);
    fprintf(stderr,"###    %s at cycle %d.\n",res->Name(),cycle);
  }
}

void INSTRUCTION_GROUP::Add_Valid_ISLOT(ISLOT* islot)
{
  valid_islots.push_back(islot);
}

void INSTRUCTION_GROUP::Set_Write_Write_Interlock()
{
  write_write_interlock = true;
}

void INSTRUCTION_GROUP::Output_II_Info(FILE* fd)
{
  int i;
  bool is_first;
  const int ii_vec_size = II_Info_Size();
  const int max_num_bad_iis = sizeof(bad_iis) * 8;
  // We need ii relative information for ii's in the range
  // 1..cycle_with_final_res_requirement.  An ii of 0 makes no sense and an II
  // enough cycles that the request doesn't need to wrap are the outside bounds.

  if ( ii_vec_size <= 0 ) {
    ii_res_req_gname.Stub_Out();
    ii_res_id_set_gname.Stub_Out();
    return;
  }

  std::vector<GNAME*> ii_res_req_gname_vector(ii_vec_size);
  std::vector<GNAME*> ii_resources_used_gname_vector(ii_vec_size);
  std::vector<bool> ii_can_do_vector(ii_vec_size);

  int greatest_bad_ii = 0;

  for ( i = 0; i < res_requirement.Max_Res_Cycle(); ++i ) {
    if ( res_requirement.Compute_Maybe_Output_II_RES_REQ(
           i+1,fd,
           ii_res_req_gname_vector[i],
           ii_resources_used_gname_vector[i])
    ) {
      ii_can_do_vector[i] = true;
    }
    else {
      ii_can_do_vector[i] = false;
      greatest_bad_ii = i;
      if ( i > max_num_bad_iis ) {
        fprintf(stderr,"### Error: bad II %d > %d.  "
                "Need a more flexible representation.\n",
                i, max_num_bad_iis);
      }
    }
  }

  for ( i = 0; i < sizeof(bad_iis) / sizeof(bad_iis[0]); ++i ) {
    bad_iis[i] = 0ULL;
  }

  for ( i = 0; i <= greatest_bad_ii; ++i ) {
    if ( ! ii_can_do_vector[i] ) {
      bad_iis[i / bits_per_long_long] |= (1ULL << (i % bits_per_long_long));
    }
  }
  
  // Print vector of pointers to the II relative resource requirements

  fprintf(fd,"static const SI_RR %s[] = {",
          ii_res_req_gname.Gname());

  is_first = true;
  for ( i = 0; i < ii_vec_size; ++i ) {
    Maybe_Print_Comma(fd,is_first);
    if ( ii_can_do_vector[i] )
      fprintf(fd,"\n  %s",ii_res_req_gname_vector[i]->Gname());
    else
      fprintf(fd,"\n  0");
  }

  fprintf(fd,"\n};\n");

  // Print vector of pointers to the II relative resoruce id sets

  fprintf(fd,"static const SI_RESOURCE_ID_SET * const %s[] = {",
          ii_res_id_set_gname.Gname());

  is_first = true;
  for ( i = 0; i < ii_vec_size; ++i ) {
    Maybe_Print_Comma(fd,is_first);
    if ( ii_can_do_vector[i] ) {
      fprintf(fd,"\n  %s",
              ii_resources_used_gname_vector[i]->Gname());
    }
    else
      fprintf(fd,"\n  0");
  }

  fprintf(fd,"\n};\n");

}

void INSTRUCTION_GROUP::Output_Latency_Info(FILE* fd)
{
  operand_latency_info.Output(fd);
  result_latency_info.Output(fd);
}

void INSTRUCTION_GROUP::Output_Issue_Slot_Info(FILE* fd)
{
  if ( valid_islots.size() == 0 ) {

/* Comment out the warning until the beast skewed support is implemented;
 * it's currently a post 7.2 affair.
 *
 *  if ( ISLOT::Count() > 0 )
 *    fprintf(stderr,"### Issue slots defined but none defined for %s\n",name);
*/
    islot_vec_gname.Stub_Out();
    return;
  }

  fprintf(fd,"static SI_ISSUE_SLOT * const %s[] = {",islot_vec_gname.Gname());

  bool is_first = true;
  std::list<ISLOT*>::iterator i;
  for (i = valid_islots.begin(); i != valid_islots.end(); ++i) {
    ISLOT* islot = *i;

    Maybe_Print_Comma(fd,is_first);
    fprintf(fd,"\n  %s",islot->Addr_Of_Gname());
  }

  fprintf(fd,"\n};\n");
}

void INSTRUCTION_GROUP::Output(FILE* fd)
{
  int i;

  fprintf(fd,"\n/* Instruction group %s */\n",name);
  res_requirement.Output(fd);
  res_requirement.Compute_Output_Resource_Count_Vec(fd);
  Output_II_Info(fd);
  Output_Latency_Info(fd);
  Output_Issue_Slot_Info(fd);


  fprintf(fd,"static SI %s = {\n",gname.Gname());
  fprintf(fd,"  \"%s\",\n",name);
  fprintf(fd,"  %-15d, /* id */\n",id);
  fprintf(fd,"  %-15s, /* operand latency */\n",
             operand_latency_info.Gname());
  fprintf(fd,"  %-15s, /* result latency */\n",
             result_latency_info.Gname());
  fprintf(fd,"  %-15d, /* load access time */\n",
             load_access_time);
  fprintf(fd,"  %-15d, /* last issue cycle */\n",
             last_issue_cycle);
  fprintf(fd,"  %-15d, /* store available time */\n",
             store_available_time);
  fprintf(fd,"  %-15s, /* resource requirement */\n",
             res_requirement.Gname());
  fprintf(fd,"  %-15s, /* res id used set vec */\n",
             res_requirement.Res_Id_Set_Gname());
  fprintf(fd,"  %-15d, /* II info size */\n",
             II_Info_Size() >= 0 ? II_Info_Size() : 0);
  fprintf(fd,"  %-15s, /* II resource requirement vec */\n",
             ii_res_req_gname.Gname());
  fprintf(fd,"  %-15s, /* II res id used set vec */\n",
             ii_res_id_set_gname.Gname());
  fprintf(fd,"  {{");
  for ( i = 0; i < sizeof(bad_iis) / sizeof(bad_iis[0]); ++i ) {
    fprintf(fd, "0x%llx", bad_iis[i]);
    if ( i < sizeof(bad_iis) / sizeof(bad_iis[0]) - 1 ) fprintf(fd, ",");
  }
  fprintf(fd, "}}    , /* Bad IIs */\n");
  fprintf(fd,"  %-15d, /* valid issue slots vec size */\n",
             valid_islots.size());
  fprintf(fd,"  %-15s, /* valid issue slots vec */\n",
             islot_vec_gname.Gname());
  fprintf(fd,"  %-15d, /* resource count vec size */\n",
             res_requirement.Res_Count_Vec_Size());
  fprintf(fd,"  %-15s, /* resource count vec */\n",
             res_requirement.Res_Count_Vec_Gname());
  fprintf(fd,"  %-15s  /* write-write interlock */\n",
             write_write_interlock ? "1" : "0");
  fprintf(fd,"};\n");
}

void INSTRUCTION_GROUP::Output_All(FILE* fd)
{
  std::list<INSTRUCTION_GROUP*>::iterator iig;

  for (iig = instruction_groups.begin();
       iig != instruction_groups.end();
       ++iig
  ) {
    (*iig)->Output(fd);
  }

  fprintf(fd,"SI * const SI_ID_si[] = {");

  bool is_first = true;
  for (iig = instruction_groups.begin();
       iig != instruction_groups.end();
       ++iig
  ) {
    Maybe_Print_Comma(fd,is_first);
    fprintf(fd,"\n  %s",(*iig)->Addr_Of_Gname());
  }

  fprintf(fd,"\n};\n");

  fprintf(fd,"const int SI_ID_count = %d;\n",count);

  fprintf(fd,"\n"); // One extra new line to separate from what follows.

}

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

/////////////////////////////////////
class TOP_SCHED_INFO_MAP {
/////////////////////////////////////
// Keeps track of which TOPs need are in which INSTRUCTION_GROUPs (and thus
// map to which TOP_SCHED_INFOs.
/////////////////////////////////////

public:
  static void Add_Entry( TOP top, INSTRUCTION_GROUP* ig );
  // Add entry to the map.  <top> uses <ig>'s scheduling information.

  static void Output( FILE* fd );
  // Write out the map.

  static void Create_Dummies( void );
  // Create schedling info for the "dummy" instructions.

private:
  static std::vector<char*> top_sched_info_ptr_map;  // The map itself.
  static std::vector<bool> top_sched_info_defined;   // Which elements defined
};

std::vector<char*> TOP_SCHED_INFO_MAP::top_sched_info_ptr_map(TOP_count,"0");
std::vector<bool> TOP_SCHED_INFO_MAP::top_sched_info_defined(TOP_count,false);

void TOP_SCHED_INFO_MAP::Create_Dummies( void )
{
  INSTRUCTION_GROUP *dummies = NULL;

  for ( int i = 0; i < TOP_count; ++i ) {
    if ( TOP_is_dummy((TOP)i) ) {
      if ( !dummies ) {
	dummies = new INSTRUCTION_GROUP("Dummy instructions");
	dummies->Set_Any_Operand_Access_Time(0);
	dummies->Set_Any_Result_Available_Time(0);
      }
      top_sched_info_ptr_map[i] = dummies->Addr_Of_Gname();
    }
  }
}

void TOP_SCHED_INFO_MAP::Add_Entry( TOP top, INSTRUCTION_GROUP* ig )
{
  if ( top_sched_info_defined[(int) top] ) {
    fprintf(stderr,"### Warning: scheduling information for %s redefined.\n",
            TOP_Name(top));
  }

  top_sched_info_ptr_map[(int) top] = ig->Addr_Of_Gname();
  top_sched_info_defined[(int) top] = true;
}

void TOP_SCHED_INFO_MAP::Output( FILE* fd )
{
  int i;

  fprintf(fd,"SI * const SI_top_si[%d] = {",TOP_count);

  bool err = false;
  bool is_first = true;
  for ( i = 0; i < TOP_count; ++i ) {
    bool isa_member = ISA_SUBSET_Member(machine_isa, (TOP)i);
    bool is_dummy = TOP_is_dummy((TOP)i);

    Maybe_Print_Comma(fd,is_first);

    fprintf(fd,"\n  %-10s  /* %s */",top_sched_info_ptr_map[i],
                                     TOP_Name((TOP)i));

    if ( top_sched_info_defined[i] ) {
      if ( ! isa_member ) {
	fprintf(stderr,"### Warning: scheduling info for non-%s ISA opcode %s\n",
                       ISA_SUBSET_Name(machine_isa), TOP_Name((TOP)i));
      } else if ( is_dummy ) {
	fprintf(stderr,"### Warning: scheduling info for dummy opcode %s\n",
                       TOP_Name((TOP)i));
      }
    } else {
      if ( isa_member && ! is_dummy ) {  
	fprintf(stderr,"### Error: no scheduling info for opcode %s\n",
                       TOP_Name((TOP)i));
	err = true;
      }
    }
  }
  fprintf(fd,"\n};\n");
  if (err) exit(EXIT_FAILURE);
}

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

// The client interface functions

static INSTRUCTION_GROUP* current_instruction_group;

/*ARGSUSED*/
void Machine(char* name, ISA_SUBSET isa, int argc, char** argv)
{
  machine_isa = isa;

  TOP_SCHED_INFO_MAP::Create_Dummies();
}

RESOURCE RESOURCE_Create(char* name, int count)
{
  return new RES(name,count);
}

ISSUE_SLOT ISSUE_SLOT_Create(char* name, int skew, int count)
{
  return new ISLOT(name,skew,count);
}

void Instruction_Group(char* name,...)
{
  va_list ap;
  TOP opcode;

  current_instruction_group = new INSTRUCTION_GROUP(name);

  va_start(ap,name);

  while ( (opcode = static_cast<TOP>(va_arg(ap,int))) != TOP_UNDEFINED )
    TOP_SCHED_INFO_MAP::Add_Entry(opcode,current_instruction_group);

  va_end(ap);
}

void Any_Operand_Access_Time( int time )
{
  current_instruction_group->Set_Any_Operand_Access_Time(time);
}

void Operand_Access_Time( int operand_index, int time )
{
  current_instruction_group->Set_Operand_Access_Time(operand_index,time);
}

void Any_Result_Available_Time( int time )
{
  current_instruction_group->Set_Any_Result_Available_Time(time);
}

void Result_Available_Time( int result_index, int time )
{
  current_instruction_group->Set_Result_Available_Time(result_index,time);
}

void Load_Access_Time( int time )
{
  current_instruction_group->Set_Load_Access_Time(time);
}

void Last_Issue_Cycle( int time )
{
  current_instruction_group->Set_Last_Issue_Cycle(time);
}

void Store_Available_Time( int time )
{
  current_instruction_group->Set_Store_Available_Time(time);
}

void Resource_Requirement( RESOURCE resource, int time )
{
  current_instruction_group->Add_Resource_Requirement(resource,time);
}

void Valid_Issue_Slot( ISSUE_SLOT slot )
{
  current_instruction_group->Add_Valid_ISLOT(slot);
}

void Write_Write_Interlock()
{
  current_instruction_group->Set_Write_Write_Interlock();
}

void Machine_Done( char* filename )
{
  FILE* fd = fopen(filename,"w");

  if ( fd == NULL ) {
    fprintf(stderr,"### Error: couldn't write %s\n",filename);
    return;
  }

  fprintf(fd,"#include \"ti_si.h\"\n");
  RES::Output_All(fd);
  RES_WORD::Output_All(fd);
  ISLOT::Output_All(fd);
  INSTRUCTION_GROUP::Output_All(fd);
  TOP_SCHED_INFO_MAP::Output(fd);

  //  Print_End_Boiler_Plate(fd);

  fclose(fd);
}
