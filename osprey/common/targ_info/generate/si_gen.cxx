/*
 * Copyright (C) 2010 Advanced Micro Devices, Inc.  All Rights Reserved.
 */

/*
 * Copyright (C) 2007 PathScale, LLC.  All Rights Reserved.
 */
/*
 * Copyright 2003, 2004, 2005, 2006 PathScale, Inc.  All Rights Reserved.
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

//  $Revision: 1.6 $
//  $Date: 04/12/21 14:57:26-08:00 $
//  $Author: bos@eng-25.internal.keyresearch.com $
//  $Source: /home/bos/bk/kpro64-pending/common/targ_info/generate/SCCS/s.si_gen.cxx $


#include <assert.h>
#include <stdio.h>
#include <unistd.h>
#include <limits.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <list>
#include <map>
#include <set>
#include <string>
#include <vector>

#include "topcode.h"
#include "targ_isa_properties.h"
#include "targ_isa_subset.h"
#include "targ_isa_operands.h"
#include "gen_util.h"
#include "si_gen.h"

// Parameters:
const int bits_per_long = 32;
const int bits_per_long_long = 64;
const bool use_long_longs = true;       // For now always
const int max_operands = ISA_OPERAND_max_operands;
const int max_results = ISA_OPERAND_max_results;
const int max_machine_slots = 16;

static int current_machine_slot;
static ISA_SUBSET machine_isa[max_machine_slots];
static std::string machine_name[max_machine_slots];

static const char * const interface[] = {
  "/* ====================================================================",
  " * ====================================================================",
  " *",
  " * Description:",
  " *",
  " *   Raw processor-specific scheduling information.",
  " *",
  " *   Clients should access this information through the public interface",
  " *   defined in \"ti_si.h\".  See that interface for more detailed",
  " *   documentation.",
  " *",
  " *   The following variables are exported:",
  " *",
  " *   const SI_RRW SI_RRW_initializer",
  " *       Initial value (no resources reserved) for resource reservation",
  " *       entry.",
  " *",
  " *   const SI_RRW SI_RRW_overuse_mask",
  " *       Mask used to determine if a resource reservation entry has an",
  " *       overuse.",
  " *",
  " *   const INT SI_resource_count",
  " *       Count of elements in SI_resources array.",
  " *",
  " *   const SI_RESOURCE* const SI_resources[n]",
  " *       Fixed-size array of SI_RESOURCE records.",
  " *",
  " *   const SI SI_all[m]",
  " *       Fixed-size array of all SI records.",
  " *",
  " *   const SI_MACHINE si_machines[p]",
  " *       Fixed-size array of SI_MACHINE records.",
  " *",
  " *   int si_current_machine",
  " *       Global index into the si_machines array, defined here for",
  " *       convenience.",
  " *",
  " * ====================================================================",
  " * ====================================================================",
  " */",
  NULL
};

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
  GNAME(const char* prefix);
  // Generate a unique name.  Force a particular prefix.
  GNAME(const GNAME& other);
  // Generate a name that is a copy of <other>.  The name will not be unique.
  // Really only useful when <other> is about to be destructed, but we still
  // need to refer to it.
  const char* Gname() const;
  // Return the name.  This is the name under which the object is defined.
  const char* Addr_Of_Gname() const;
  // Return a pointer to the named object.
  void Stub_Out();
  // We've decided not to define the object after all but we may still want a
  // pointer to it.  After this call, Addr_Of_Gname will return 0.
  static GNAME Stub_Gname();
  // Return pre-built stub name.

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

GNAME::GNAME(const char* prefix) : stubbed(false) {
  assert(strlen(prefix) <= 8);
  sprintf(gname,"&%s%d",prefix,count++);
}

GNAME::GNAME(const GNAME& other) : stubbed(other.stubbed) {
  sprintf(gname,"%s",other.gname);
}

const char* GNAME::Gname() const {
  if (stubbed)
    return "0";
  else
    return gname + 1;
}

const char* GNAME::Addr_Of_Gname() const {
  if (stubbed)
    return "0";
  else
    return gname;
}

void GNAME::Stub_Out() {
  stubbed = true;
}

GNAME GNAME::Stub_Gname() {
  static GNAME stub_gname;
  stub_gname.Stub_Out();
  return stub_gname;
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

  static std::list<RES_WORD*> res_words;
  // List of all res_words in order.

  static int count;
  // Count of all resource words.

  static bool has_long_long_word;
  // Will we need to use long longs for resource words?

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

  if (    (use_long_longs && new_inx >= bits_per_long_long)
       || (!use_long_longs && new_inx >= bits_per_long)
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
    fprintf(fd,"const SI_RRW SI_RRW_initializer = 0x%" LL_FORMAT "x;\n",
            res_words.front()->initializer);
    fprintf(fd,"const SI_RRW SI_RRW_overuse_mask = 0x%" LL_FORMAT "x;\n",
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
  static RES* Create(const char *name, int count);
  // <name> is used for documentation and debugging.  <count> is the number of
  // elements in the class.

  static RES* Get(int id);
  // Find and return the resource with the given <id>.

  const char* Name() const { return name; }
  // Return debugging name.

  const char* Addr_Of_Gname() { return gname.Addr_Of_Gname(); }
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

  static int Total() { return total; }
  // Total number of different RESs.

private:
  const int count;          // Available per cycle
  const char* name;         // For documentation and debugging
  const int id;             // Unique numerical identifier
  GNAME gname;              // Generated symbolic name
  int word;                 // Which word in the table?
  int field_width;          // How wide the field?
  int shift_count;          // How much to shift (starting pos of the low
                            //   order bit
  static int total;         // Total number of different RESs (not the the
                            //   total of their counts, 1 for each RES)
  static std::map<int,RES*> resources;
                            // Map of all resources, ordered by their Id's

  RES(const char *name, int count);

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

RES::RES(const char *name, int count)
// constructor maintains list of all resources.
  : count(count), name(name), id(total++), gname("resource")
{
  resources[id] = this;
}

RES* RES::Create(const char *name, int count)
{
  int i;

  for ( i = 0; i < total; ++i )
    if (resources[i]->count == count && strcmp(resources[i]->name, name) == 0)
      return resources[i];

  return new RES(name,count);
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
  fprintf(fd,"const SI_RESOURCE * const SI_resources[%d] = {",total);

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
  fprintf(fd,"static const SI_RESOURCE %s = {\"%s\",%d,%d,%d,%d};\n",
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

  RES_REQ(const RES_REQ& rhs);
  // Copy constructor.

  bool Add_Resource(const RES* res, int cycle);
  // Require an additional resource <res> at the given <cycle> relative to
  // my start.  Return indication of success.  If adding the resource would
  // create an overuse, don't add it and return false.

  void Output(FILE* fd);
  // Output my definition and initialization.

  const char* Addr_Of_Gname() const;
  // Return name of pointer to me (in generated code).

  const char* Gname() const;
  // Return my name (in generated code).
  // The RES_REQ must be Output first.

  bool Compute_Maybe_Output_II_RES_REQ(int ii, FILE* fd,
                                       GNAME*& res_req_gname,
                                       GNAME*& resource_id_set_gname);
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

  int Max_Res_Cycle() const { return max_res_cycle; }
  // Return the cycle (relative to my start) of the latest resource I
  // require.  (Used to know how many II relative resource requirements need
  // to be computed/output.)

  void Compute_Output_Resource_Count_Vec(FILE* fd);
  // Count up all the resources of each kind that I require (in all my cycles)
  // and output a definition and initialization.

  const char* Res_Count_Vec_Gname() const;
  // Return name of pointer to start of my resource count vector.

  int Res_Count_Vec_Size() const { return res_count_vec_size; }
  // Return length of my resource count vector.

  const char* Res_Id_Set_Gname() const;
  // Return name of pointer to start of vector of resource id sets, one per
  // cycle.

  friend bool operator < (const RES_REQ& lhs, const RES_REQ& rhs);
  // Comparison operator for std::map.

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

    CYCLE_RES(const CYCLE_RES& rhs) : cycle(rhs.cycle), res_id(rhs.res_id) {}
    // Copy constructor for use by STL map.

    int Cycle() const { return cycle; }
    // Return cycle component.

    RES* Res() const { return RES::Get(res_id); }
    // Return resource component.

    friend bool operator < (const CYCLE_RES& a, const CYCLE_RES& b)
    // Ordering for map.
    {
      return    (a.cycle < b.cycle)
             || (a.cycle == b.cycle && a.res_id < b.res_id);
    }

  private:
    const short cycle;
    const short res_id;
  };

  typedef std::map<CYCLE_RES,int> CYCLE_RES_COUNT_MAP;
  // For keeping track of the number of resources of a given type in a given
  // cycle.  <cycle,res> => count

  int max_res_cycle;
  // Latest cycle with a resource requirement

  CYCLE_RES_COUNT_MAP cycle_res_count;
  // <cycle,res> -> count required

  GNAME res_count_vec_gname;
  // Symbolic name of my resource count vector.

  int res_count_vec_size;
  // How big it is.

  bool Compute_II_RES_REQ(int ii, RES_REQ& ii_res_req);

  static std::map<RES_REQ,GNAME> res_req_name_map;
  // Map of already-printed RES_REQ instances to names.

  typedef std::vector<unsigned long long> RES_ID_SET;

  struct res_id_set_cmp {
    bool operator () (const RES_ID_SET* lhs, const RES_ID_SET* rhs) const {
      if (lhs == 0 && rhs != 0)
        return true;
      else if (rhs == 0)
        return false;
      return *lhs < *rhs;  // std::lexicographical_compare
    }
  };
  typedef std::map<RES_ID_SET*,GNAME,res_id_set_cmp> RES_ID_SET_NAME_MAP;
  // Map of RES_ID_SET pointers to names.

  static std::vector<RES_ID_SET*> all_res_id_sets;
  // List of all allocated resource id sets.

  static RES_ID_SET_NAME_MAP res_id_set_name_map;
  // Map of weak RES_ID_SET references to names.

  RES_ID_SET* res_used_set_ptr;
  // Weak reference to res_used_set for this RES_REQ.

  typedef std::map<int,int> RES_COUNT_VEC;
  // Actually a map of resource ids to counts.

  struct res_count_vec_cmp {
    bool operator () (const RES_COUNT_VEC* lhs, const RES_COUNT_VEC* rhs) const {
      if (lhs == 0 && rhs != 0)
        return true;
      else if (rhs == 0)
        return false;
      return *lhs < *rhs;  // std::map lexicographical_compare
    }
  };
  typedef std::map<RES_COUNT_VEC*,GNAME,res_count_vec_cmp> RES_COUNT_NAME_MAP;

  static std::vector<RES_COUNT_VEC*> all_res_count_vecs;
  // List of all allocated resource count vectors.

  static RES_COUNT_NAME_MAP res_count_name_map;
  // Map of RES_COUNT_VEC weak references to names.

  RES_COUNT_VEC* res_count_vec_ptr;
  // Weak reference to res_count_vec for this RES_REQ.
};

std::map<RES_REQ,GNAME> RES_REQ::res_req_name_map;
std::vector<RES_REQ::RES_ID_SET*> RES_REQ::all_res_id_sets;
RES_REQ::RES_ID_SET_NAME_MAP RES_REQ::res_id_set_name_map;
std::vector<RES_REQ::RES_COUNT_VEC*> RES_REQ::all_res_count_vecs;
RES_REQ::RES_COUNT_NAME_MAP RES_REQ::res_count_name_map;

RES_REQ::RES_REQ()
  : max_res_cycle(-1)
{}

RES_REQ::RES_REQ(const RES_REQ& rhs)
  : max_res_cycle(rhs.max_res_cycle),
    cycle_res_count(rhs.cycle_res_count),
    res_count_vec_gname(rhs.res_count_vec_gname),
    res_count_vec_size(rhs.res_count_vec_size)
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

const char* RES_REQ::Addr_Of_Gname() const
{
  return res_req_name_map[*this].Addr_Of_Gname();
}

const char* RES_REQ::Gname() const
{
  return res_req_name_map[*this].Gname();
}

const char* RES_REQ::Res_Count_Vec_Gname() const
{
  if ( res_count_vec_size == 0 )
    return "0";

  return res_count_name_map[res_count_vec_ptr].Gname();
}

const char* RES_REQ::Res_Id_Set_Gname() const
{
  if ( max_res_cycle < 0 )
    return "0";

  return res_id_set_name_map[res_used_set_ptr].Gname();
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
                                              GNAME*& res_id_set_gname_ref)
{
  RES_REQ ii_res_req;

  if ( ! Compute_II_RES_REQ(ii,ii_res_req) )
    return false;

  ii_res_req.Output(fd);
  res_req_gname = new GNAME(res_req_name_map[ii_res_req]);
  if ( max_res_cycle < 0 )
    res_id_set_gname_ref = new GNAME(GNAME::Stub_Gname());
  else
    res_id_set_gname_ref = new GNAME(res_id_set_name_map[res_used_set_ptr]);
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

  if ( res_count_vec_size == 0 )
    return;

  // Avoid printing duplicate RES_REQ definitions.
  RES_COUNT_NAME_MAP::iterator rcmi = res_count_name_map.find(&res_inx_count);
  if ( rcmi == res_count_name_map.end() ) {
    // Allocate and save a copy of the local res_inx_count variable.
    RES_COUNT_VEC* res_inx_copy = new RES_COUNT_VEC(res_inx_count);
    all_res_count_vecs.push_back(res_inx_copy);
    res_count_vec_ptr = res_inx_copy;

    // Generate a name and add it to the map.
    GNAME gname;
    res_count_name_map[res_inx_copy] = gname;

    fprintf(fd,"static const SI_RESOURCE_TOTAL %s[] = {", gname.Gname());

    bool is_first = true;
    RES_COUNT_VEC::iterator mj;
    for (mj = res_inx_count.begin(); mj != res_inx_count.end(); ++mj) {
      RES* res = RES::Get((*mj).first);  // You'd think STL would allow
      int count = (*mj).second;          // something less ugly!  But no.

      Maybe_Print_Comma(fd,is_first);
      fprintf(fd,"\n  {%s,%d} /* %s */",
              RES::Get(res->Id())->Addr_Of_Gname(),count,res->Name());
    }
    fprintf(fd,"\n};\n");
  }
  else
    res_count_vec_ptr = rcmi->first;
}

void RES_REQ::Output(FILE* fd)
{
  int i;
  CYCLE_RES_COUNT_MAP::iterator mi;
  RES_ID_SET res_vec((size_t) max_res_cycle + 1,0);
  RES_ID_SET res_used_set((size_t) max_res_cycle + 1,0);

  for (mi = cycle_res_count.begin(); mi != cycle_res_count.end(); ++mi) {
    int cycle = (*mi).first.Cycle();  // You'd think this could be abstracted,
    RES* res = (*mi).first.Res();     // but I couldn't even explain the
    long long  count = (*mi).second;  // the concept to Alex S.

    res_vec[cycle] += count << res->Shift_Count();
    res_used_set[cycle] |= 1ll << res->Id();
  }

  // Avoid printing duplicate RES_REQ definitions.
  std::map<RES_REQ,GNAME>::iterator rrmi = res_req_name_map.find(*this);
  if ( rrmi == res_req_name_map.end() ) {
    // Generate a name and add it to the map.
    GNAME gname("res_req");
    res_req_name_map[*this] = gname;

    fprintf(fd,"static const SI_RRW %s[%d] = {\n  %d",
            gname.Gname(),
            max_res_cycle + 2,
            max_res_cycle + 1);

    for ( i = 0; i <= max_res_cycle; ++i )
      fprintf(fd,",\n  0x%" LL_FORMAT "x",res_vec[i]);

    fprintf(fd,"\n};\n");
  }

  if ( max_res_cycle < 0 )
    return;

  // Avoid printing duplicate resource id sets.
  RES_ID_SET_NAME_MAP::iterator rsmi = res_id_set_name_map.find(&res_used_set);
  if ( rsmi == res_id_set_name_map.end() ) {
    // Allocate and save a copy of the local res_used_set variable.
    RES_ID_SET* res_set_copy = new RES_ID_SET(res_used_set);
    all_res_id_sets.push_back(res_set_copy);
    res_used_set_ptr = res_set_copy;

    // Generate a name and add it to the map.
    GNAME gname;
    res_id_set_name_map[res_set_copy] = gname;

    fprintf(fd,"static const SI_RESOURCE_ID_SET %s[%d] = {", gname.Gname(),
            max_res_cycle + 1);

    bool is_first = true;
    for ( i = 0; i <= max_res_cycle; ++i ) {
      Maybe_Print_Comma(fd,is_first);
      fprintf(fd,"\n  0x%" LL_FORMAT "x",res_used_set[i]);
    }

    fprintf(fd,"\n};\n");
  }
  else
    res_used_set_ptr = rsmi->first;
}

bool operator < (const RES_REQ& lhs, const RES_REQ& rhs)
{
  // Check for differing max_res_cycle values.
  if (lhs.max_res_cycle != rhs.max_res_cycle)
    return lhs.max_res_cycle < rhs.max_res_cycle;

  // Compute the res_vec vector as in RES_REQ::Output.
  std::vector<unsigned long long> res_vec_lhs((size_t) lhs.max_res_cycle + 1,0);

  RES_REQ::CYCLE_RES_COUNT_MAP::const_iterator mi;
  for (mi = lhs.cycle_res_count.begin(); mi != lhs.cycle_res_count.end(); ++mi)
  {
    int cycle = (*mi).first.Cycle();
    RES* res = (*mi).first.Res();
    long long count = (*mi).second;

    res_vec_lhs[cycle] += count << res->Shift_Count();
  }

  std::vector<unsigned long long> res_vec_rhs((size_t) rhs.max_res_cycle + 1,0);
  for (mi = rhs.cycle_res_count.begin(); mi != rhs.cycle_res_count.end(); ++mi)
  {
    int cycle = (*mi).first.Cycle();
    RES* res = (*mi).first.Res();
    long long count = (*mi).second;

    res_vec_rhs[cycle] += count << res->Shift_Count();
  }

  // Compare values in res_vec vectors.
  int i;
  for ( i = 0; i <= lhs.max_res_cycle; ++i )
    if (res_vec_lhs[i] != res_vec_rhs[i])
      return res_vec_lhs[i] < res_vec_rhs[i];

  // The two RES_REQ instances will be identical in output.
  return false;
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
  ISLOT(const char* name, int skew, int avail_count);
  // <name> is for documentation and debugging.  <skew> gives a latency skew
  // instructions issued in me.

  const char* Addr_Of_Gname() { return gname.Addr_Of_Gname(); }
  // Return pointer to my name in generated.

  static void Output_Data(FILE* fd, int machine_slot);
  // Output all the issue slots and a vector of pointers to them all.

  static void Output_Members(FILE* fd, int machine_slot);
  // Output the count of issue slots and a pointer to the vector.

private:
  const char* name;             // User supplied for documentation & debugging
  const int skew;               // Latency skew
  const int avail_count;        // How many instructions can happen in it
  GNAME gname;                  // Symbolic name in generated

  static std::list<ISLOT*> islots[max_machine_slots];
  // All the created islot lists.

  static int count[max_machine_slots];
  // How many issue slots in each list?
};

std::list<ISLOT*> ISLOT::islots[max_machine_slots];
int ISLOT::count[max_machine_slots] = { 0 };

ISLOT::ISLOT(const char* name, int skew, int avail_count)
  : name(name),
    skew(skew),
    avail_count(avail_count)
{
  islots[current_machine_slot].push_back(this);
  ++count[current_machine_slot];
}

void ISLOT::Output_Data(FILE* fd, int machine_slot)
{
  std::list<ISLOT*>::iterator isi;

  for ( isi = islots[machine_slot].begin();
        isi != islots[machine_slot].end();
        ++isi
  ) {
    ISLOT* islot = *isi;
    fprintf(fd,"static const SI_ISSUE_SLOT %s = { \"%s\",%d,%d};\n",
            islot->gname.Gname(),
            islot->name,
            islot->skew,
            islot->avail_count);
  }

  if ( count[machine_slot] == 0 )
    fprintf(fd, "\n"
            "static const SI_ISSUE_SLOT * const SI_issue_slots_%d[1] = {0};\n",
            machine_slot);
  else {
    fprintf(fd,"\nstatic const SI_ISSUE_SLOT * const SI_issue_slots_%d[%d] = {",
            machine_slot, count[machine_slot]);

    bool is_first = true;
    for ( isi = islots[machine_slot].begin();
          isi != islots[machine_slot].end();
          ++isi
    ) {
      ISLOT* islot = *isi;
      Maybe_Print_Comma(fd,is_first);
      fprintf(fd,"\n  %s",islot->Addr_Of_Gname());
    }

    fprintf(fd,"\n};\n");
  }
}

void ISLOT::Output_Members(FILE* fd, int machine_slot)
{
  fprintf(fd,"    %-20d  /* SI_issue_slot_count */,\n",count[machine_slot]);
  fprintf(fd,"    SI_issue_slots_%-5d  /* si_issue_slots */,\n",machine_slot);
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

  LATENCY_INFO(const LATENCY_INFO& rhs);
  // Copy constructor

  void Set_Any_Time(int time);
  // Any (all) of the operands or results have <time> as access or available
  // time.

  void Set_Time(int index, int time);
  // <index>'th operand or result has <time> as access or available time.

  void Output(FILE* fd);
  // Output latency vector to <fd>.

  const char* Gname() const;
  // Return name of pointer to me in generated file.
  // The latency vector must be output first.

  friend bool operator < (const LATENCY_INFO& a, const LATENCY_INFO& b);
  // Comparison operator for std::map.

private:
  const int max_elements;       // Maximum number of operands or results
  bool any_time_defined;        // Overriding time defined
  int any_time;                 // And here it is
  std::vector<bool> times_defined;   // Times for each operands defined?
  std::vector<int> times;            // And here they are

  static std::map<LATENCY_INFO,GNAME> output_latencies;
};

std::map<LATENCY_INFO,GNAME> LATENCY_INFO::output_latencies;

LATENCY_INFO::LATENCY_INFO(int max_elements)
  : max_elements(max_elements),
    any_time_defined(false),
    times_defined(max_elements,false),
    times(max_elements)
{}

LATENCY_INFO::LATENCY_INFO(const LATENCY_INFO& rhs)
  : max_elements(rhs.max_elements),
    any_time_defined(rhs.any_time_defined),
    any_time(rhs.any_time),
    times_defined(rhs.times_defined),
    times(rhs.times)
{}

void LATENCY_INFO::Set_Any_Time(int time)
{
  if ( any_time_defined ) {
    fprintf(stderr,"### Warning any_time redefined for latency.  "
                   "Was %d.  Is %d\n",
                   any_time,
                   time);
  }

  any_time_defined = true;
  any_time = time;
}

void LATENCY_INFO::Set_Time(int index, int time)
{
  if ( any_time_defined ) {
    fprintf(stderr,"### WARNING: latency setting specific time after any time.  "
                   "Any %d.  Specific %d\n",
                   any_time,
                   time);
  }

  assert(index < max_elements);

  if ( times_defined[index] ) {
    fprintf(stderr,"### WARNING: Resetting latency time.  "
                   "Was %d. Now is %d\n",
                   time,
                   times[index]);
  }

  times_defined[index] = true;
  times[index] = time;
}

void LATENCY_INFO::Output(FILE* fd)
{
  // Avoid output of duplicate latencies.
  std::map<LATENCY_INFO,GNAME>::iterator lmi = output_latencies.find(*this);
  if ( lmi != output_latencies.end() )
    return;

  // Generate a name and add it to the map.
  GNAME gname("latency");
  output_latencies[*this] = gname;

  fprintf(fd,"static const mUINT8 %s[%lu] = {",gname.Gname(),times.size());

    bool is_first = true;
    std::vector<int>::iterator i;
    for ( i = times.begin(); i < times.end(); ++i ) {
      Maybe_Print_Comma(fd,is_first);
      fprintf(fd,"%d",any_time_defined ? any_time : *i);
    }

    fprintf(fd,"};\n");
}

const char* LATENCY_INFO::Gname() const
{
  return output_latencies[*this].Gname();
}

bool operator < (const LATENCY_INFO& a, const LATENCY_INFO& b)
{
  if ( a.times.size() != b.times.size() )
    return a.times.size() < b.times.size();

  for (int i = 0; i < a.times.size(); ++i) {
    int t_a = a.any_time_defined ? a.any_time : a.times[i];
    int t_b = b.any_time_defined ? b.any_time : b.times[i];
    if ( t_a != t_b )
      return t_a < t_b;
  }

  // The two latencies will be identical in output.
  return false;
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
  INSTRUCTION_GROUP(const char* name);
  void Set_Any_Operand_Access_Time(int time);
  void Set_Operand_Access_Time(int operand_index, int time);
  void Set_Any_Result_Available_Time(int time);
  void Set_Result_Available_Time(int result_index, int time);
  void Set_Load_Access_Time( int time );
  void Set_Last_Issue_Cycle( int time );
  void Set_Store_Available_Time( int time );
  void Add_Resource_Requirement(const RES* res, int cycle);
  void Add_Alternative_Resource_Requirement(const RES* res, int cycle);
  void Add_Valid_ISLOT(ISLOT* islot);
  void Set_Write_Write_Interlock();

  static void Output_All(FILE* fd);
  // Write them all out

  static void Output_Data(FILE* fd, int machine_slot);
  // Write out per-machine list of pointers.

  static void Output_Members(FILE* fd, int machine_slot);
  // Write out count and pointer to data.

  unsigned int Id();
  // Unique id (available only after call to Output_All).

  static unsigned int Count();
  // Number of unique SI records (available only after call to Output_All).

private:
  int id;                               // Index in vector of same
  const char* name;                     // User supplied name for documentation
  RES_REQ res_requirement;              // Required to issue
  RES_REQ alternative_res_requirement;  // Alternative resource if
                                        // res_requirement cannot be satisified

  std::list<ISLOT*> valid_islots;       // If there are any issue slots at all
  GNAME islot_vec_gname;                // Variable name of above in generated

  LATENCY_INFO operand_latency_info;    // When operands latch
  LATENCY_INFO result_latency_info;     // When results available

  int load_access_time;                 // When loads access memory
  int last_issue_cycle;                 // Last issue cycle in simulated insts
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
  // All the defined instruction groups (may have duplicates).

  static std::list<INSTRUCTION_GROUP*>
    by_machine_instruction_groups[max_machine_slots];
  // List of instruction groups for each machine.

  static int by_machine_count[max_machine_slots];
  // Count of instruction groups for each machine.

  int II_Info_Size() const { return res_requirement.Max_Res_Cycle(); }
  // Latest cycle in which I need a resource

  void Output_II_Info(FILE* fd);
  void Output_Latency_Info(FILE* fd);
  void Output_Issue_Slot_Info(FILE* fd);
  void Output_Members(FILE* fd);
  void Output(FILE* fd) const;

  struct instruction_group_cmp {
    bool operator () (const INSTRUCTION_GROUP* lhs,
                      const INSTRUCTION_GROUP* rhs) const
    {
      if (lhs == NULL && rhs != NULL)
        return true;
      else if (rhs == NULL)
        return false;

      // Compare fields as in Output member function.

#ifdef Is_True_On
      // name
      if (lhs->name == NULL && rhs->name != NULL)
        return true;
      else if (rhs->name == NULL)
        return false;
      if (strcmp(lhs->name, rhs->name) != 0)
        return strcmp(lhs->name, rhs->name) < 0;
#endif

      // operand_latency_info
      if (strcmp(lhs->operand_latency_info.Gname(),
                 rhs->operand_latency_info.Gname()) != 0)
        return strcmp(lhs->operand_latency_info.Gname(),
                      rhs->operand_latency_info.Gname()) < 0;

      // result_latency_info
      if (strcmp(lhs->result_latency_info.Gname(),
                 rhs->result_latency_info.Gname()) != 0)
        return strcmp(lhs->result_latency_info.Gname(),
                      rhs->result_latency_info.Gname()) < 0;

      // load_access_time
      if (lhs->load_access_time != rhs->load_access_time)
        return lhs->load_access_time < rhs->load_access_time;

      // last_issue_cycle
      if (lhs->last_issue_cycle != rhs->last_issue_cycle)
        return lhs->last_issue_cycle < rhs->last_issue_cycle;

      // store_available_time
      if (lhs->store_available_time != rhs->store_available_time)
        return lhs->store_available_time < rhs->store_available_time;

      // res_requirement
      if (strcmp(lhs->res_requirement.Gname(),
                 rhs->res_requirement.Gname()) != 0)
        return strcmp(lhs->res_requirement.Gname(),
                      rhs->res_requirement.Gname()) < 0;

      // alternative_res_requirement
      if (strcmp(lhs->alternative_res_requirement.Gname(),
                 rhs->alternative_res_requirement.Gname()) != 0)
        return strcmp(lhs->alternative_res_requirement.Gname(),
                      rhs->alternative_res_requirement.Gname()) < 0;

      // Res_Id_Set_Gname, II_Info_Size: redundant (res_requirement)

      // ii_res_req_gname
      if (strcmp(lhs->ii_res_req_gname.Gname(),
                 rhs->ii_res_req_gname.Gname()) != 0)
        return strcmp(lhs->ii_res_req_gname.Gname(),
                      rhs->ii_res_req_gname.Gname()) < 0;

      // ii_res_id_set_gname
      if (strcmp(lhs->ii_res_id_set_gname.Gname(),
                 rhs->ii_res_id_set_gname.Gname()) != 0)
        return strcmp(lhs->ii_res_id_set_gname.Gname(),
                      rhs->ii_res_id_set_gname.Gname()) < 0;

      // bad_iis
      int i;
      for (i = 0; i < sizeof(lhs->bad_iis) / sizeof(lhs->bad_iis[0]); ++i)
        if (lhs->bad_iis[i] != rhs->bad_iis[i])
          return lhs->bad_iis[i] < rhs->bad_iis[i];

      // valid_islots
      if (lhs->valid_islots.size() != rhs->valid_islots.size())
        return lhs->valid_islots.size() < rhs->valid_islots.size();

      // islot_vec_gname
      if (strcmp(lhs->islot_vec_gname.Gname(),
                 rhs->islot_vec_gname.Gname()) != 0)
        return strcmp(lhs->islot_vec_gname.Gname(),
                      rhs->islot_vec_gname.Gname()) < 0;

      // Res_Count_Vec_Size, Res_Count_Vec_Gname: redundant (res_requirment)

      // write_write_interlock
      if (lhs->write_write_interlock != rhs->write_write_interlock)
        return lhs->write_write_interlock < rhs->write_write_interlock;

      return false;
    }
  };
  typedef std::set<INSTRUCTION_GROUP*,instruction_group_cmp>
    INSTRUCTION_GROUP_SET;

  static INSTRUCTION_GROUP_SET instruction_group_set;
  // Set of all unique instruction group objects.
};


std::list<INSTRUCTION_GROUP*> INSTRUCTION_GROUP::instruction_groups;

std::list<INSTRUCTION_GROUP*>
  INSTRUCTION_GROUP::by_machine_instruction_groups[max_machine_slots];

INSTRUCTION_GROUP::INSTRUCTION_GROUP_SET
  INSTRUCTION_GROUP::instruction_group_set;

INSTRUCTION_GROUP::INSTRUCTION_GROUP(const char* name)
    : name(name),
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
  by_machine_instruction_groups[current_machine_slot].push_back(this);
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

void INSTRUCTION_GROUP::Add_Alternative_Resource_Requirement(const RES* res, int cycle)
{
  if (! alternative_res_requirement.Add_Resource(res,cycle)) {
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

  unsigned int j;
  for ( j = 0; j < sizeof(bad_iis) / sizeof(bad_iis[0]); ++j ) {
    bad_iis[j] = 0ULL;
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

void INSTRUCTION_GROUP::Output_Members(FILE* fd)
{
  unsigned int i;

  res_requirement.Output(fd);
  res_requirement.Compute_Output_Resource_Count_Vec(fd);
  alternative_res_requirement.Output(fd); 
  alternative_res_requirement.Compute_Output_Resource_Count_Vec(fd);
  Output_II_Info(fd);
  Output_Latency_Info(fd);
  Output_Issue_Slot_Info(fd);

  // Keep track of duplicate instruction group data.
  INSTRUCTION_GROUP_SET::iterator mi = instruction_group_set.find(this);
  if (mi == instruction_group_set.end()) {
    instruction_group_set.insert(this);
  }
}

void INSTRUCTION_GROUP::Output(FILE* fd) const
{
  unsigned int i;

  assert(id != 0);

  fprintf(fd,"  {                  /* SI id %u */\n",id);
#ifdef Is_True_On
  fprintf(fd,"    \"%s\",\n",name);
#endif
  fprintf(fd,"    %-15s, /* operand latency */\n",
             operand_latency_info.Gname());
  fprintf(fd,"    %-15s, /* result latency */\n",
             result_latency_info.Gname());
  fprintf(fd,"    %-15d, /* load access time */\n",
             load_access_time);
  fprintf(fd,"    %-15d, /* last issue cycle */\n",
             last_issue_cycle);
  fprintf(fd,"    %-15d, /* store available time */\n",
             store_available_time);
  fprintf(fd,"    %-15s, /* resource requirement */\n",
             res_requirement.Gname());
  fprintf(fd,"    %-15s, /* alternative resource requirement */\n",
             alternative_res_requirement.Gname());
  fprintf(fd,"    %-15s, /* res id used set vec */\n",
             res_requirement.Res_Id_Set_Gname());
  fprintf(fd,"    %-15d, /* II info size */\n",
             II_Info_Size() >= 0 ? II_Info_Size() : 0);
  fprintf(fd,"    %-15s, /* II resource requirement vec */\n",
             ii_res_req_gname.Gname());
  fprintf(fd,"    %-15s, /* II res id used set vec */\n",
             ii_res_id_set_gname.Gname());
  fprintf(fd,"    {{");
  for ( i = 0; i < sizeof(bad_iis) / sizeof(bad_iis[0]); ++i ) {
    fprintf(fd, "0x%" LL_FORMAT "x", bad_iis[i]);
    if ( i < sizeof(bad_iis) / sizeof(bad_iis[0]) - 1 ) fprintf(fd, ",");
  }
  fprintf(fd, "}}    , /* bad IIs */\n");
  fprintf(fd,"    %-15d, /* valid issue slots vec size */\n",
             (unsigned int) valid_islots.size());
  fprintf(fd,"    %-15s, /* valid issue slots vec */\n",
             islot_vec_gname.Gname());
  fprintf(fd,"    %-15d, /* resource count vec size */\n",
             res_requirement.Res_Count_Vec_Size());
  fprintf(fd,"    %-15s, /* resource count vec */\n",
             res_requirement.Res_Count_Vec_Gname());
  fprintf(fd,"    %-15s  /* write-write interlock */\n",
             write_write_interlock ? "1" : "0");
  fprintf(fd,"  }");
}

void INSTRUCTION_GROUP::Output_All(FILE* fd)
{
  std::list<INSTRUCTION_GROUP*>::iterator iig;
  INSTRUCTION_GROUP_SET::iterator mi;
  unsigned int i;

  for (iig = instruction_groups.begin();
       iig != instruction_groups.end();
       ++iig
  ) {
    (*iig)->Output_Members(fd);
  }

  i = 1;
  fprintf(fd,"\nconst SI SI_all[%lu] = {\n", instruction_group_set.size());
  for (mi = instruction_group_set.begin();
       mi != instruction_group_set.end();
       ++mi
  ) {
    if (i > 1)
      fprintf(fd,",\n");
    (*mi)->id = i;
    (*mi)->Output(fd);
    i++;
  }
  fprintf(fd,"\n};\n");
}

void INSTRUCTION_GROUP::Output_Data(FILE* fd, int machine_slot)
{
  std::list<INSTRUCTION_GROUP*>::iterator iig;

  fprintf(fd,"\nstatic const int SI_ID_si_%d[%lu] = {",machine_slot,
          by_machine_instruction_groups[machine_slot].size());

  bool is_first = true;
  for (iig = by_machine_instruction_groups[machine_slot].begin();
       iig != by_machine_instruction_groups[machine_slot].end();
       ++iig
  ) {
    Maybe_Print_Comma(fd,is_first);
    fprintf(fd,"\n  %u",(*iig)->Id());
  }

  fprintf(fd,"\n};\n");

  fprintf(fd,"\n"); // One extra new line to separate from what follows.
}

void INSTRUCTION_GROUP::Output_Members(FILE* fd, int machine_slot)
{
  fprintf(fd,"    %-20lu  /* SI_ID_count */,\n",
          by_machine_instruction_groups[machine_slot].size());
  fprintf(fd,"    SI_ID_si_%-11d  /* SI_ID_si */,\n",machine_slot);
}

unsigned int INSTRUCTION_GROUP::Id()
{
  // Use the id from the representative instruction group object.
  INSTRUCTION_GROUP_SET::iterator mi = instruction_group_set.find(this);
  assert(mi != instruction_group_set.end());
  return (*mi)->id;
}

unsigned int INSTRUCTION_GROUP::Count()
{
  return instruction_group_set.size();
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

  static void Output_Data( FILE* fd , int machine_slot );
  // Write out the map.

  static void Output_Members( FILE* fd, int machine_slot );
  // Write out a pointer to the map.

  static void Create_Dummies( void );
  // Create scheduling info for the "dummy" instructions.

private:
  static std::vector<INSTRUCTION_GROUP*> top_sched_info_ptr_map[max_machine_slots];
  // Map to instruction group instances.

  static std::vector<bool> top_sched_info_defined[max_machine_slots];
  // Which elements defined

  static INSTRUCTION_GROUP* machine_dummies[max_machine_slots];
  // Pointer to dummy instruction used to fill unused slots
};

std::vector<INSTRUCTION_GROUP*>
  TOP_SCHED_INFO_MAP::top_sched_info_ptr_map[max_machine_slots];
std::vector<bool>
  TOP_SCHED_INFO_MAP::top_sched_info_defined[max_machine_slots];

INSTRUCTION_GROUP* TOP_SCHED_INFO_MAP::machine_dummies[max_machine_slots];

void TOP_SCHED_INFO_MAP::Create_Dummies( void )
{
  INSTRUCTION_GROUP *dummies = NULL;

  top_sched_info_ptr_map[current_machine_slot].resize(TOP_count,NULL);
  top_sched_info_defined[current_machine_slot].resize(TOP_count,false);

  for ( int i = 0; i < TOP_count; ++i ) {
    if ( TOP_is_dummy((TOP)i) ) {
      if ( !dummies ) {
        dummies = new INSTRUCTION_GROUP("Dummy instructions");
        dummies->Set_Any_Operand_Access_Time(0);
        dummies->Set_Any_Result_Available_Time(0);
      }
      top_sched_info_ptr_map[current_machine_slot][i] = dummies;
    }
  }
  machine_dummies[current_machine_slot] = dummies;
}

void TOP_SCHED_INFO_MAP::Add_Entry( TOP top, INSTRUCTION_GROUP* ig )
{
  if ( top_sched_info_defined[current_machine_slot][(int) top] ) {
    fprintf(stderr,"### Warning: scheduling information for %s redefined.\n",
            TOP_Name(top));
  }

  top_sched_info_ptr_map[current_machine_slot][(int) top] = ig;
  top_sched_info_defined[current_machine_slot][(int) top] = true;
}

void TOP_SCHED_INFO_MAP::Output_Data( FILE* fd, int machine_slot )
{
  int i;

  fprintf(fd,"static const int SI_top_si_%d[%d] = {",machine_slot,TOP_count);

  bool err = false;
  bool is_first = true;
  for ( i = 0; i < TOP_count; ++i ) {
    bool isa_member = ISA_SUBSET_Member(machine_isa[machine_slot], (TOP)i);
    bool is_dummy = TOP_is_dummy((TOP)i);

    if ( top_sched_info_defined[machine_slot][i] ) {
      if ( ! isa_member ) {
        fprintf(stderr,
                "### Warning: scheduling info for non-%s ISA opcode %s (%s)\n",
                ISA_SUBSET_Name(machine_isa[machine_slot]),
                TOP_Name((TOP)i),
                machine_name[machine_slot].c_str());
      } else if ( is_dummy ) {
        fprintf(stderr,
                "### Warning: scheduling info for dummy opcode %s (%s)\n",
                TOP_Name((TOP)i),
                machine_name[machine_slot].c_str());
      }

    } else {
      if ( isa_member && ! is_dummy ) {  
        fprintf(stderr,
                "### Error: no scheduling info for opcode %s for machine %s\n",
                TOP_Name((TOP)i),
                machine_name[machine_slot].c_str());
        err = true;
      }
    }

    // If we have seen a fatal error, skip printing the entry to avoid a crash.
    if ( ! err ) {
      Maybe_Print_Comma(fd,is_first);
      if ( ! isa_member )
        fprintf(fd,"\n  %-4u  /* %s (dummy, not in ISA subset %s) */",
                machine_dummies[machine_slot]->Id(),TOP_Name((TOP)i),
                ISA_SUBSET_Name(machine_isa[machine_slot]));
      else
        fprintf(fd,"\n  %-4u  /* %s */",
                top_sched_info_ptr_map[machine_slot][i]->Id(),TOP_Name((TOP)i));
    }
  }
  fprintf(fd,"\n};\n");
  if (err) exit(EXIT_FAILURE);
}

void TOP_SCHED_INFO_MAP::Output_Members( FILE* fd, int machine_slot )
{
  fprintf(fd,"    SI_top_si_%-10d  /* SI_top_si */\n",machine_slot);
}

//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////

// The client interface functions

static INSTRUCTION_GROUP* current_instruction_group;

void Targ_SI( void )
{
  current_machine_slot = 0;
}

void Machine( const char* name, ISA_SUBSET isa )
{
  machine_name[current_machine_slot] = name;
  machine_isa[current_machine_slot] = isa;

  TOP_SCHED_INFO_MAP::Create_Dummies();
}

RESOURCE RESOURCE_Create(const char* name, int count)
{
  return RES::Create(name,count);
}

ISSUE_SLOT ISSUE_SLOT_Create(const char* name, int skew, int count)
{
  return new ISLOT(name,skew,count);
}

void Instruction_Group(const char* name,...)
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

void Alternative_Resource_Requirement( RESOURCE resource, int time )
{
  current_instruction_group->Add_Alternative_Resource_Requirement(resource,time);
}

void Valid_Issue_Slot( ISSUE_SLOT slot )
{
  current_instruction_group->Add_Valid_ISLOT(slot);
}

void Write_Write_Interlock( void )
{
  current_instruction_group->Set_Write_Write_Interlock();
}

void Machine_Done( void )
{
  current_machine_slot++;
  if (current_machine_slot == max_machine_slots) {
    fprintf(stderr,"### Error: max_machine_slots is %d\n",max_machine_slots);
    exit(EXIT_FAILURE);
  }
}

void Targ_SI_Done( const char* basename )
{
  std::string h_filename(basename);
  std::string c_filename(basename);

  h_filename.append(".h");
  c_filename.append(".c");

  FILE* hfile = fopen(h_filename.c_str(),"w");
  FILE* cfile = fopen(c_filename.c_str(),"w");

  if ( hfile == NULL ) {
    fprintf(stderr,"### Error: couldn't write %s\n",h_filename.c_str());
    return;
  }

  if ( cfile == NULL ) {
    fprintf(stderr,"### Error: couldn't write %s\n",c_filename.c_str());
    return;
  }

  Emit_Header(hfile, basename, interface);

  fprintf(hfile,"#include \"ti_si_types.h\"\n");

  fprintf(cfile,"#include \"%s\"\n", h_filename.c_str());

  bool is_first = true;
  int i;

  // output global data
  RES::Output_All(cfile);
  RES_WORD::Output_All(cfile);
  INSTRUCTION_GROUP::Output_All(cfile);

  // output per-machine data
  for (i = 0; i < current_machine_slot; ++i) {
    ISLOT::Output_Data(cfile, i);
    INSTRUCTION_GROUP::Output_Data(cfile, i);
    TOP_SCHED_INFO_MAP::Output_Data(cfile, i);
  }

  fprintf(hfile,"\nextern const SI_RRW SI_RRW_initializer;\n");
  fprintf(hfile,"extern const SI_RRW SI_RRW_overuse_mask;\n");

  fprintf(hfile,"\nextern const INT SI_resource_count;\n");
  fprintf(hfile,"extern const SI_RESOURCE* const SI_resources[%d];\n",
          RES::Total());

  fprintf(hfile,"\nextern const SI SI_all[%u];\n",INSTRUCTION_GROUP::Count());

  fprintf(hfile,"\nextern const SI_MACHINE si_machines[%d];\n",
          current_machine_slot);

  fprintf(cfile,"\nconst SI_MACHINE si_machines[%d] = {",current_machine_slot);

  // output SI_MACHINE members
  for (i = 0; i < current_machine_slot; ++i) {
    std::string quoted_name("\"");
    quoted_name.append(machine_name[i]);
    quoted_name.append("\"");

    Maybe_Print_Comma(cfile, is_first);
    fprintf(cfile,"\n  {\n");
    fprintf(cfile,"    %-20s  /* name */,\n", quoted_name.c_str());
    ISLOT::Output_Members(cfile, i);
    INSTRUCTION_GROUP::Output_Members(cfile, i);
    TOP_SCHED_INFO_MAP::Output_Members(cfile, i);
    fprintf(cfile,"  }");
  }

  fprintf(cfile,"\n};\n");

  fprintf(hfile,"\nextern int si_current_machine;\n");

  fprintf(cfile,"\nint si_current_machine;   /* index into si_machines */\n");

  Emit_Footer(hfile);

  fclose(hfile);
  fclose(cfile);
}

