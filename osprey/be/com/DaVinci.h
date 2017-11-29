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


/* -*-Mode: c++;-*- (Tell emacs to use c++ mode) */
/* ====================================================================
 *
 * Module: DaVinci.h
 * $Revision: 1.8 $
 * $Date: 05/12/05 08:59:12-08:00 $
 * $Author: bos@eng-24.pathscale.com $
 * $Source: /scratch/mee/2.4-65/kpro64-pending/be/com/SCCS/s.DaVinci.h $
 *
 * Description:
 *	Interface to daVinci.  The DaVinci class, along with its
 *      subordinate classes and types, provides a high level model
 *      for interactive examination of graph-based data structures.
 *      Sufficient sanity checking is done to detect most daVinci
 *      usage errors before issuing commands to daVinci.
 *      
 *      Key concepts and interface functions are described below:
 *
 *   daVinci  X-Window visualization tool from University of Bremen, Germany.
 *            runs as a separate process. Commands are sent to daVinci via
 *            a UNIX pipe, replies (ack, events) are returned though another
 *            pipe.
 *            see  http://www.informatik.uni-bremen.de/~davinci/
 *
 *   DA_ACK    return type indicating a reply from daVinci.
 *             NULL => ok; otherwise an error message (const char *).
 *             Assume message is statically allocated and may be overwritten
 *             by any subsequence DaVinci member function.
 *
 *   DaVinci(FILE *trace_fp = NULL)
 *      create DaVinci object.  if trace_fp != NULL then all traffic
 *      to/from daVinci is logged in indicated file.
 *
 *   bool Davinci::enabled(bool msg)
 *      use environment variable DAVINCIHOME as indication daVinci
 *      availability.  return true iff set; if not set and msg is true
 *      then issue (at most one) warning.
 *
 *   void DaVinci::Event_Loop(DaVinci_Callback *cb_hook)
 *   void DaVinci::Exit_Event_Loop()
 *      respond to actions from daVinci window until exit action taken.
 *         daVinci file menu - exit daVinci
 *         daVinci edit menu - exit_event_loop
 *         in callback function, call exit_event_loop.
 *      The first closes the daVinci window while the later two allow
 *      the caller to resume w/o closing the DaVinci window.  If the
 *      DaVinci object is not destroyed, and the daVinci is not exited,
 *      subsequence calls to event_loop() will resume response to actions.
 *      see notes on DaVinci_Callback below.
 *
 *   DA_ACK DaVinci::Title(const char *title)
 *   DA_ACK DaVinci::Show_Status(const char *status)
 *   DA_ACK DaVinci::Show_Message(const char *msg)
 *      Display strings at top, lower left, lower right resp. of window.
 *      The daVinci doc. suggests using "message" for transient events
 *      such as "file saved" and "status" for ongoing state.  "title"
 *      seems like a good place to identify the origin of the DaVinci
 *      client.
 *
 *   struct MENU_INFO
 *      for defining submenu under "edit".  suggest that this be done
 *      using static initialization.  A menu item may identify a submenu
 *      or be itself a leave item.  The initially_active field indicates
 *      if the item is accessable on immediately after menu_create().
 *               
 *   DA_ACK DaVinci::Menu_Create(INT n_items, const MENU_INFO *items)
 *      create new menu hierarchy under the daVinci "edit" menu.
 *      see MENU_INFO structure.
 *
 *   DA_ACK DaVinci::Menu_Activate(INT n_ids, const char *ids[])
 *   DA_ACK DaVinci::Menu_Deactivate(INT n_ids, const char *ids[])
 *      enable/disable sets of menu items.
 *   
 *   void   DaVinci::Graph_Begin()
 *   void   DaVinci::Node_Begin( id, label, node_type )
 *   void   DaVinci::Out_Edge( edge_id, edge_type, dest_id )
 *   void   DaVinci::Node_End()
 *   DA_ACK DaVinci::Graph_End()
 *      The above define a graph.  Each node as a unique NODE_ID;
 *      each edge a uniq EDGE_ID.
 *      Usage:  graph_begin [ node_begin out_edge* node_end ]* graph_end
 *      see also: NODE_ID, EDGE_ID, NODE_TYPE, EDGE_TYPE
 *
 *   DA_ACK DaVinci::Change_Attr( node_id, node_type, new_label = NULL )
 *   DA_ACK DaVinci::Change_Attr( edge_id, edge_type )
 *      change attributes (color, shape, line-type, etc.) of node/edge.
 *
 *   void   DaVinci::Update_Begin()
 *   void   DaVinci::New_Node( node_id, label, node_type )
 *   void   DaVinci::New_Edge( edge_id, edge_type, src, dst )
 *   DA_ACK DaVinci::Update_End()
 *      add new nodes/edges.  must list all new nodes before any
 *      new edges in an update group.  i.e., usage symopsis is:
 *         Update_Begin New_Node* New_Edge* Update_End
 *
 * class DaVinci_Callback
 *    virtual void DaVinci_Callback::node_select( n_ids, id_array )
 *    virtual void DaVinci_Callback::edge_select( edge_id )
 *    virtual void DaVinci_Callback::menu_select( menu_id )
 *
 *    Response to daVinci events is managented by Callback functions.
 *    The DaVinci class client creates a subclass of DaVinci_Callback
 *    and redefines the virtual member for events of interest.
 *    The Callback subclass object is passed to event_loop().
 *   
 * ====================================================================
 */

//more: open use MEM_POOL * or keep 'generic' for use outside Mongoose?

#ifndef DaVinci_INCLUDED
#define DaVinci_INCLUDED

// Avoid compile errors for STL files
//
#if defined(defs_INCLUDED) && !defined(USE_STANDARD_TYPES)
#undef short
#undef int
#undef long
#endif // defined(defs_INCLUDED) && !defined(USE_STANDARD_TYPES)

#include <stdio.h>
#include <assert.h>
#include <sys/types.h> 
#include <queue>        // STL queue.
#if (__GNUC__==2)
#include <hash_map>
using std::hash_map;
#else
#include <ext/hash_map>     // STL hash_map.
using __gnu_cxx::hash_map;
using __gnu_cxx::hash;
#endif
#include <set>          // STL set.

#include "defs.h"
#include "errors.h"
#include "cxx_memory.h"

typedef void *NODE_ID;
#define MAX_VIEW_NUM  64

struct EDGE_ID {   // if multiple edges add occurence id.
  NODE_ID src;
  NODE_ID dst;

  EDGE_ID(NODE_ID s, NODE_ID d) : src(s), dst(d) {}
};

// WARNING: Do _NOT_ delete strings/arrays passed back by Callback
//          functions.  These are allocated/deleted by the DaVinci
//          class in accordance with its lifetime.

class DaVinci_Callback {
public:
  virtual void Node_Select(const INT n_ids, const NODE_ID id_array[]);
  virtual void Edge_Select(const EDGE_ID& edge_id);
  virtual void Menu_Select(const char *menu_id);
};

class DaVinci;

typedef enum {
  NS_UNSET,     // default is BOX.
  NS_BOX,
  NS_CIRCLE,
  NS_ELLIPSE,
  NS_RHOMBUS,
  NS_TEXT
  //more NS_ICON
} NODE_SHAPE;

typedef enum {
  NB_UNSET,     // default is SINGLE.
  NB_SINGLE,
  NB_DOUBLE
} NODE_BORDER;

typedef enum {
  NH_UNSET,      // default is SHOW.
  NH_HIDE,
  NH_SHOW
} NODE_HIDE;

class NODE_TYPE {
private:
  char         _type_name[20];  // node type name.   "" if unset.
  char         _node_color[30]; // attribute: COLOR  "" if unset.
  NODE_SHAPE   _node_shape;     // attribute: _GO
  NODE_BORDER  _border;         // attribute: BORDER
  NODE_HIDE    _hide;           // attribute: HIDDEN
  // more: fontfamily,fontstyle,iconfile.

  friend class DaVinci;
public:
  NODE_TYPE() {
    _type_name[0]  = '\0';
    _node_color[0] = '\0';
    _node_shape    = NS_UNSET;
    _border        = NB_UNSET;
    _hide          = NH_UNSET;
  }
  NODE_TYPE& Name(const char *typ_name);
  NODE_TYPE& Color(const char *rgb);
  NODE_TYPE& Shape(NODE_SHAPE ns) {
    _node_shape = ns; return *this;
  }
  NODE_TYPE& Boarder(NODE_BORDER nb) {
    _border = nb; return *this;
  }
  NODE_TYPE& Hidden(NODE_HIDE nh) {
    _hide = nh; return *this;
  }
};

typedef enum {
  EP_UNSET,   // default is SOLID.
  EP_SOLID,
  EP_DOTTED,
  EP_DASHED,
  EP_THICK,   // beware: same as a selected edge.
  EP_DOUBLE
} EDGE_PATTERN;

typedef enum {
  ED_UNSET,   // default is NORMAL (->).
  ED_NORMAL,
  ED_INVERSE,
  ED_BOTH,
  ED_NONE
} EDGE_DIR;

class EDGE_TYPE {
private:
  char         _type_name[20];    // edge type name.         "" if unset.
  char         _edge_color[30];   // attribute: EDGECOLOR    "" if unset.
  EDGE_PATTERN _edge_pattern;     // attribute: EDGEPATTERN
  EDGE_DIR     _edge_dir;         // attribute: _DIR

  friend class DaVinci;
public:
  EDGE_TYPE() {
    _type_name[0]  = '\0';
    _edge_color[0] = '\0';
    _edge_pattern  = EP_UNSET;
    _edge_dir      = ED_UNSET;
  }
  EDGE_TYPE& Name(const char *typ_name);
  EDGE_TYPE& Color(const char *rgb);
  EDGE_TYPE& Pattern(EDGE_PATTERN pat) { _edge_pattern = pat; return *this; }
  EDGE_TYPE& Direction(EDGE_DIR dir)   { _edge_dir     = dir; return *this; }
};

struct MENU_INFO {
  const char *id;               // id from/to DaVinci.
  const char *label;            // displayed by DaVinci.
  bool       initially_active;  // active on startup ?
  INT        n_subitems;        // 0 => this is a leaf item.
  MENU_INFO *subitems;          // NULL => this is leaf item.
};

typedef enum {
  EK_COM_ERROR,
  EK_OK,
  EK_SEL_EDGE,
  EK_SEL_MENU,
  EK_SEL_NODES,
  EK_QUIT,           // DaVinci exiting.
  EK_CLOSE,
  EK_DISCONNECT
     // more: many more event kinds ..
} EVENT_KIND;

typedef enum{
  CONTEX_UNUSE, 
  CONTEX_ACTIVE
}CONTEX_TYPE;

struct EVENT_T {       //more? full OO-style - queue ptrs, use class heir ..
  EVENT_KIND  kind;
  union {
    struct {
      const char *msg;   // ptr into static buffer.
    } com_error;

    struct {
      NODE_ID edge_src;
      NODE_ID edge_dst;
    } sel_edge;

    struct {
      const char *label;  // always a heap object.
    } sel_menu;

    struct {
      INT      n_nodes;
      NODE_ID *node_ids;
    } sel_nodes;
  } u;
};

struct Equal_obj {
  bool operator()(const char* s1, const char* s2) const {
    return strcmp(s1, s2) == 0;
  }
};

typedef enum {
  DM_ACTIVE,    // want menu item turned on.
  DM_INACTIVE,  // want menu item turned on.
  DM_UNKNOWN    // entry from menu select (using map domain as string pool).
} Item_status;

typedef hash_map< const char *, Item_status,
			     hash<const char *>, Equal_obj > Item_info;

class Menu_info {
private:
  MEM_POOL *_m;
public:
  Item_info items;

  Menu_info(MEM_POOL *m) :  _m(m) {}  // alloc to pool; free with pool.
  ~Menu_info() {}
  void operator=(Menu_info&); // don't define.
  Menu_info(Menu_info&);      // don't define.

  const char *Add(const char *cp); // use items maps domain as string pool.
  void        Set(const char *cp, Item_status status);
};

typedef const char *DA_ACK;  // NULL => ok, otherwise error string.

class DaVinci {
private:
  class IO {
  private:
    FILE  *_to_fp;
    FILE  *_from_fp;
    FILE  *_trace_fp;
    bool   _trace_tagged;
  public:
    IO() {
      _to_fp = _from_fp = _trace_fp = NULL;
      _trace_tagged = false;
    }
    void Trace(FILE *tfp) { _trace_fp = tfp; }
    ~IO();

    void Init(FILE *to, FILE *from) {
      _to_fp   = to;
      _from_fp = from;
    }
    void  Close();
    void  Out_Fmt(const char *fmt, ...);
    char *In_Line();               // sync read; NULL => EOF.
  };
  typedef UINT32 FTAG;
  typedef FTAG   FTAGS;

  IO              _io;
  MEM_POOL       *_m;
  std::queue<EVENT_T>  _event_q;
  std::set<NODE_ID>    _node_def_set;  // used iff _usage_check == true.
  std::set<NODE_ID>    _node_ref_set;  // used iff _usage_check == true.
  Menu_info       _menu_state;
  bool            _basic_menu_added;
  bool            _in_event_loop;
  bool            _display_ok;
  bool            _usage_check;
  FTAG            _ftag_last;
  INT             _node_cnt;
  INT             _edge_cnt;
  pid_t           _pid;
  INT                         _contex;
  static std::queue<EVENT_T>  _event_q_socket[MAX_VIEW_NUM];
  static INT                  _tcp_socket ;
  static INT                  _davinci_count ;
  static INT                  _contex_count ;
  static CONTEX_TYPE          _contex_use_array[MAX_VIEW_NUM];
  static INT                  _current_contex ;
  static bool                 _use_socket;

  const char *Ft_Str(const FTAG ftag);
  void        Usage_Error(FTAG curr, FTAGS prereq);

  bool Usage_Ok(FTAG curr, FTAGS prereq) {
    if ( _display_ok && (prereq == 0 || (_ftag_last & prereq)) ) {
      _ftag_last = curr;
      return true;
    }
    Usage_Error(curr, prereq);
    return false;
  }
  DA_ACK Wait_For_Ack();

  DA_ACK Emit_Ack(const char *line) {
    assert( strchr( line, '\n' ) == NULL );
    _io.Out_Fmt( line );
    _io.Out_Fmt( "\n" );
    return Wait_For_Ack();
  }
  void Emit_Do(const char *line) {
    DA_ACK msg = Emit_Ack( line );
    if ( msg ) {
      fprintf(stderr, "Unexpected DaVinci error: %s\n", msg);
    }
  }
  const char *Parse_Menu_Label(const char *epfx);
  bool        Parse_Event(const char *line, EVENT_T *event);
  bool        Parse_Node_Ids(const char *epfx,
			     INT *n_nodes, NODE_ID **node_ids);

  void Emit_Menu(INT n_items, const MENU_INFO *items);
  void Emit_Attr(const NODE_TYPE& nt, const char **comma);
  void Emit_Attr(const EDGE_TYPE& et);

  void   Menu_Basic_Do( const char *label );
  DA_ACK Menu_Set_Active();

  void Kill_Davinci();
public:
  static bool enabled(bool msg) {
    bool is_enabled = ( getenv("DAVINCIHOME") != NULL );
    if(!is_enabled) {
      _use_socket = true;
      is_enabled = (getenv("UDRAWIP") != NULL);
    }
    if ( ! is_enabled ) {
      static bool msg_given = false;
      if ( ! msg_given ) {
	DevWarn("daVinci not enabled; %s\n",
		"must set DAVINCIHOME and put daVinci on path.");
	msg_given = true;
      }
    }
    return is_enabled;
  }

  DaVinci(MEM_POOL *m, FILE *trace_fp = NULL, bool usage_check = false);
  ~DaVinci();

  bool is_ok() const { return _display_ok; }

  DaVinci(const DaVinci&);        // don't allow/define.
  void operator=(const DaVinci&); // don't allow/define.

  void Event_Loop(DaVinci_Callback *cb_hook);
  void Exit_Event_Loop();

  DA_ACK Title(const char *title);        // on top.
  DA_ACK Show_Status(const char *status); // lower right.
  DA_ACK Show_Message(const char *msg);   // lower left (for transient msg).

  DA_ACK Menu_Create(INT n_items, const MENU_INFO *items);
  DA_ACK Menu_Activate(INT n_ids, const char *ids[]);
  DA_ACK Menu_Deactivate(INT n_ids, const char *ids[]);

  void   Graph_Begin();
  void   Node_Begin(NODE_ID id, const char *label, const NODE_TYPE& node_type);
  void   Out_Edge(const EDGE_ID&   edge_id,
		  const EDGE_TYPE& edge_type,
		  const NODE_ID    dest_id);
  void   Node_End();
  DA_ACK Graph_End();

  DA_ACK Change_Attr(const NODE_ID     node_id,
		     const NODE_TYPE&  nt,
		     const char       *new_label = NULL);

  DA_ACK Change_Attr(const EDGE_ID& edge_id, const EDGE_TYPE& et);

  void Update_Begin();  // all new_node() must preceed first new_edge().
  void New_Node(NODE_ID id, const char *label, const NODE_TYPE& nt );
  void New_Edge(const EDGE_ID& id, const EDGE_TYPE& et,
		NODE_ID src, NODE_ID dst);
  void Delete_Edge(const EDGE_ID& id);
  DA_ACK Update_End();
};

#endif
