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
 * Module: DaVinci.cxx
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/be/com/DaVinci.cxx,v $
 *
 * Description:
 *	Interface to daVinci, a visualization system for displaying
 *	directed graphs.
 *
 * ====================================================================
 * ====================================================================
 */


#include <sys/socket.h>
#include <arpa/inet.h>
#include <stdio.h>

#include <sys/types.h>		    // for pid_t
#include <unistd.h>		    // for fork(), pipe(), etc.
#include <signal.h>		    // for SIGINT
#ifndef __MINGW32__
#if defined(BUILD_OS_DARWIN) || defined(__CYGWIN__) || defined(__APPLE__)
#include <sys/wait.h>		    // for waitpid()
#else 
#include <wait.h>		    // for waitpid()
#endif /* defined(BUILD_OS_DARWIN) */
#endif /* __MINGW32__ */
#include <stdarg.h>                 // for varargs.
#include <time.h>
#include <string.h>
#include <errno.h>

#include "DaVinci.h"

#define MAX_MENU_LABEL_LEN  100

#define CALLBACK_DEBUG

 std::queue<EVENT_T>  DaVinci::_event_q_socket[MAX_VIEW_NUM];
 INT                  DaVinci::_tcp_socket = 0;
 INT                  DaVinci::_davinci_count =0;
 INT                  DaVinci::_contex_count = 0;
 CONTEX_TYPE          DaVinci::_contex_use_array[MAX_VIEW_NUM];
 INT                  DaVinci::_current_contex = -1;
 bool                 DaVinci::_use_socket = false;


// ------------------------------------------------------------------------
//         CALLBACK HOOKS -- default handlers.
// ------------------------------------------------------------------------

void
DaVinci_Callback::Node_Select(const INT n_ids, const NODE_ID id_array[])
{
#ifdef CALLBACK_DEBUG
  fprintf(stderr, "Node_Select([");
  const char *sep = " ";
  for (INT i = 0; i < n_ids; ++i) {
    fprintf(stderr, "%s%p", sep, id_array[i]);
    sep = ", ";
  }
  fprintf(stderr, " ])\n");
#endif
}

void
DaVinci_Callback::Edge_Select(const EDGE_ID& id)
{
#ifdef CALLBACK_DEBUG
  fprintf(stderr, "Edge_Select(%p,%p)\n", id.src, id.dst);
#endif
}

void
DaVinci_Callback::Menu_Select(const char *menu_id)
{
#ifdef CALLBACK_DEBUG
  fprintf(stderr, "Menu_Select(%s)\n", menu_id);
#endif
}

// ------------------------------------------------------------------------
//         NODE/EDGE TYPE support.
// ------------------------------------------------------------------------

NODE_TYPE&
NODE_TYPE::Name(const char *typ_name)
{
  if ( typ_name ) {
    strncpy(_type_name, typ_name, sizeof(_type_name) - 1);
  }
  // more? handle arb. long typ_name.
  // more: DaVinci 2.1? use node type database.
  return *this;
}

NODE_TYPE&
NODE_TYPE::Color(const char *rgb)
{
  strncpy(_node_color, rgb, sizeof(_node_color) - 1);
  // more: handle long color names; check color validity.
  return *this;
}

EDGE_TYPE&
EDGE_TYPE::Name(const char *typ_name)
{
  if ( typ_name ) {
    strncpy(_type_name, typ_name, sizeof(_type_name) - 1);
  }
  // more? handle arb. long typ_name.
  // more: DaVinci 2.1? use node type database.
  return *this;
}


EDGE_TYPE&
EDGE_TYPE::Color(const char *rgb)
{
  strncpy(_edge_color, rgb, sizeof(_edge_color) - 1);
  // more: handle long color names; check color validity.
  return *this;
}

// ------------------------------------------------------------------------
//        Menu_info support -- status (& string pool)
// ------------------------------------------------------------------------


const char *
Menu_info::Add(const char *cp)
{
  Item_info::iterator it = items.find( cp );

  if ( cp == NULL ) return NULL;

  if ( it != items.end() ) {
    return (*it).first;
  } else {
    char       *buf = CXX_NEW_ARRAY(char, strlen(cp)+1, _m);
    const char *cpy = strcpy( buf, cp );
    items[ cpy ] = DM_UNKNOWN;
    return cpy;
  }
}

void
Menu_info::Set(const char *cp, Item_status status)
{
  if ( cp ) {
    if ( items.find( cp ) == items.end() ) {
      char *buf = CXX_NEW_ARRAY(char, strlen(cp)+1, _m);
      cp = strcpy( buf, cp );
    }
    items[ cp ] = status;
  }
}

// ------------------------------------------------------------------------
//         DaVinci::IO  low level IO to/from DaVinci
// ------------------------------------------------------------------------

DaVinci::IO::~IO()
{
    if(!_use_socket || _davinci_count==0) Close ();
}

void
DaVinci::IO::Close()
{
  if ( _to_fp )   (void)fclose( _to_fp   );
  if ( _from_fp ) (void)fclose( _from_fp );
}

void
DaVinci::IO::Out_Fmt(const char *fmt, ...)
{
  if ( _to_fp == NULL ) {
    fprintf(stderr, "DaVinci::IO::Out_Fmt _to_fp not set!\n");
    return;
  }
  va_list ap;
  va_start(ap, fmt);

  vfprintf(_to_fp, fmt, ap);  // send to DaVinci.

  if ( _trace_fp ) {
    if ( ! _trace_tagged ) {
      fprintf(_trace_fp, "TO-DAVINCI: ");
      _trace_tagged = true;
    }
    vfprintf(_trace_fp, fmt, ap);

    if ( strchr( fmt, '\n' ) ) {  // assume linefeeds only in fmt.
      _trace_tagged = false;
    }
    fflush(_trace_fp);
  }

  va_end(ap);
}

char *
DaVinci::IO::In_Line()
{
  static char buf[10000];   //more: deal with arb. long lines ..

  if ( _from_fp == NULL ) {
    fprintf(stderr, "DaVinci::IO::Out_Fmt _from_fp not set!\n");
    return NULL;
  }
  char *rp = fgets( buf, sizeof(buf), _from_fp );

  if ( rp ) {
    char *np = strchr( rp, '\n' );
    if ( np ) {
      *np = '\0';
    } else {
      fprintf(stderr, "in_line truncation! (%.50s ..)\n", buf);
    }
    if(_use_socket){
      np = strchr( rp, '\r' );
      if ( np ) {
        *np = '\0';
      } 
    }

    if (strlen(rp) >= sizeof(buf) ) {
      fprintf(stderr, "INTERNAL ERROR! DaVinci::IO:in_line buf overflow\n");
      abort();
    }
  }
  if ( _trace_fp && rp ) {
    fprintf(_trace_fp, "FROM-DAVINCI: %s\n", buf);
    fflush(_trace_fp);

    _trace_tagged = false;  // read whole lines from DaVinci.
  }
  return rp;
}

// ------------------------------------------------------------------------
//         DaVinci private members.
// ------------------------------------------------------------------------

#define FT_DAVINCI           FTAG(1 <<  0)
#define FT_TITLE             FTAG(1 <<  1)
#define FT_SHOW_STATUS       FTAG(1 <<  2)
#define FT_SHOW_MESSAGE      FTAG(1 <<  3)
#define FT_MENU_CREATE       FTAG(1 <<  4)
#define FT_MENU_ACTIVATE     FTAG(1 <<  5)
#define FT_MENU_DEACTIVATE   FTAG(1 <<  6)
#define FT_GRAPH_BEGIN       FTAG(1 <<  7)
#define FT_NODE_BEGIN        FTAG(1 <<  8)
#define FT_OUT_EDGE          FTAG(1 <<  9)
#define FT_NODE_END          FTAG(1 << 10)
#define FT_GRAPH_END         FTAG(1 << 11)
#define FT_CHANGE_ATTR       FTAG(1 << 12)
#define FT_UPDATE_BEGIN      FTAG(1 << 13)
#define FT_NEW_NODE          FTAG(1 << 14)
#define FT_NEW_EDGE          FTAG(1 << 15)
#define FT_DELETE_EDGE       FTAG(1 << 16)
#define FT_UPDATE_END        FTAG(1 << 17)

#define BASE_SET ( \
   FT_DAVINCI | FT_TITLE | FT_SHOW_STATUS | FT_SHOW_MESSAGE  \
 | FT_MENU_CREATE | FT_MENU_ACTIVATE | FT_MENU_DEACTIVATE    \
 | FT_GRAPH_END | FT_CHANGE_ATTR | FT_UPDATE_END             \
 )

const char *
DaVinci::Ft_Str(const FTAG ftag)
{
  const char *s = "<<ft_str: unknown tag>>";

  switch ( ftag ) {
  case FT_DAVINCI:          s = "DaVinci";           break;
  case FT_TITLE:            s = "title";             break;
  case FT_SHOW_STATUS:      s = "show_status";       break;
  case FT_SHOW_MESSAGE:     s = "show_message";      break;
  case FT_MENU_CREATE:      s = "menu_create";       break;
  case FT_MENU_ACTIVATE:    s = "menu_activate";     break;
  case FT_MENU_DEACTIVATE:  s = "menu_deactivate";   break;
  case FT_GRAPH_BEGIN:      s = "graph_begin";       break;
  case FT_NODE_BEGIN:       s = "node_begin";        break;
  case FT_OUT_EDGE:         s = "out_edge";          break;
  case FT_NODE_END:         s = "node_end";          break;
  case FT_GRAPH_END:        s = "graph_end";         break;
  case FT_CHANGE_ATTR:      s = "change_attr";       break;
  case FT_UPDATE_BEGIN:     s = "update_begin";      break;
  case FT_NEW_NODE:         s = "new_node";          break;
  case FT_NEW_EDGE:         s = "new_edge";          break;
  case FT_UPDATE_END:       s = "update_end";        break;
  default:
    ; // unknown tag.
  }
  return s;
}

void
DaVinci::Usage_Error(FTAG curr, FTAGS prereq)
{
  fprintf(stderr, "Error while Calling DaVinci::%s - ", Ft_Str(curr));

  if ( ! _display_ok ) {
    fprintf(stderr, "DaVinci display not ok\n");
  } else {
    fprintf(stderr, "preceeding %s expected member of {", Ft_Str(_ftag_last));
    FTAGS fts   = prereq;
    const char *comma = "";
    for (FTAGS ft = FT_DAVINCI; fts != 0; ft <<= 1) {
      if ( ft & fts ) {
	fprintf(stderr, "%s %s", comma, Ft_Str(ft));
	comma = ",";
	fts ^= ft;
      }
    }
    fprintf(stderr, " }\n");
  }
}

DA_ACK
DaVinci::Wait_For_Ack()
{
  EVENT_T  event;
  char    *line;

  // NOTE: never expect to queue an OK or COM_ERROR because Wait_For_Ack()
  //       is called at the end of each command to DaVinci.  Therefore
  //       must look for OK/COM_ERROR in the unparsed input stream.  Any
  //       other events from davinci are queued for processing in event_loop().
  //       If this assumption is proven invalid will need to alter protocol.
  //       In particular, should keep a side queue for OK/COM_ERROR msgs ..

  while ( (line = _io.In_Line()) != NULL ) {
    if(_use_socket){
      if(line[0] == 'c' && line[1] == 'o' && line[2] == 'n' && line[3] == 't'
        && line[4] == 'e' && line[5] == 'x' && line[6] == 't'){
        if(line[9] >='0' && line[9] <= '9'){
          sscanf(line, "context(\"%d\")", &_current_contex);
        }
        continue;
      }
    }
    if ( Parse_Event( line, &event ) ) {
      switch ( event.kind ) {
      case EK_OK:
	return NULL;
      case EK_COM_ERROR:
	return event.u.com_error.msg;
      default:
        if(_use_socket){
          _event_q_socket[_current_contex].push( event );
        }else{
	   _event_q.push( event );
        }
      }
    }
  }
  _display_ok = false;

  return "Unexpected EOF from DaVinci";
}

bool
DaVinci::Parse_Node_Ids(const char *epfx, INT *n_nodes, NODE_ID **node_ids)
{
  INT         n_alloc = 5;
  NODE_ID    *ids     = CXX_NEW_ARRAY(NODE_ID, n_alloc, _m);
  INT         cnt     = 0;
  const char *cp      = epfx;
  const char *sp;
  NODE_ID     id;

  // To avoid warnings, get id lists from the MEM_POOL whence other
  // DaVinci storage comes.  Since the amount of storage required
  // will generally be small, don't bother attempting to reuse/reclaim
  // before the pool is popped or deleted.

  if ( cp[0] != '(' || cp[1] != '[' ) {        // (["id","id"..])
    fprintf(stderr, "BAD NODE_ID list (lp): %s\n", epfx);
    return false;
  }
  cp += 2;

  while ( *cp != ']' ) {
    sp = strchr( cp, ',' );
    if ( sp == NULL ) {
      sp = strchr( cp, ']' );
      if ( sp == NULL ) {
	fprintf(stderr, "BAD NODE_ID list (sep): %s\n", epfx);
	return false;
      }
    }
    if ( sscanf( cp, "\"%p\"", &id ) != 1 ) {
      fprintf(stderr, "BAD NODE_ID (id): .. %s\n", cp);
      return false;
    }
    if ( cnt >= n_alloc ) {    // more: improve storage management.
      n_alloc = cnt + 10;
      NODE_ID *nids = CXX_NEW_ARRAY(NODE_ID, n_alloc, _m);
      for (INT i = 0; i < cnt; ++i) {
	nids[i] = ids[i];
      }
      ids = nids;
    }
    ids[ cnt++ ] = id;

    cp = ( *sp == ',' ? sp + 1 : sp );
  }
  *n_nodes  = cnt;
  *node_ids = ids;

  return true;
}

bool
Parse_Edge_Id( const char *epfx, EVENT_T *event ) // ("node_id:node_id")
{
  if ( sscanf(epfx, "(\"%p:%p\")",
	      &event->u.sel_edge.edge_src,
	      &event->u.sel_edge.edge_dst) != 2 ) {
    fprintf(stderr, "Malformed EDGE_ID %s\n", epfx);
    return false;
  }
  return true;
}

const char *
DaVinci::Parse_Menu_Label( const char *epfx ) // ("label")
{
  char  label[MAX_MENU_LABEL_LEN];
  INT   len   = strlen( epfx );

  if ( epfx[0] != '('
       || epfx[1] != '"'
       || epfx[len - 2] != '"'
       || epfx[len - 1] != ')' ) {
    fprintf(stderr, "parse_menu_label: not wrapped as expected\n");
    return NULL;
  }
  strncpy( label, epfx + 2, len - 4 );
  label[ len - 4 ] = '\0';

  // more: would be nice to have a string pool that accepts substrings
  //       w/o needing to copy before testing for membership in pool.

  return  _menu_state.Add( label );  // use as string pool.
}

static struct {
  const char *name;     // order by name - for binary search.
  EVENT_KIND  kind;
} Event_Tbl[] = {
  { "close",                   EK_CLOSE   },
  { "communication_error",     EK_COM_ERROR },
  { "disconnect",              EK_DISCONNECT},
  { "edge_selection_label",    EK_SEL_EDGE  },
  { "menu_selection",          EK_SEL_MENU  },
  { "node_selections_labels",  EK_SEL_NODES },
  { "ok",                      EK_OK        },
  { "quit",                    EK_QUIT      }
};
#define N_EVENT   ( sizeof(Event_Tbl) / sizeof(Event_Tbl[0]) )

bool
DaVinci::Parse_Event(const char *line, EVENT_T *event)
{
  const char *epfx;  // immediately after '(' or at end of reply.

  epfx = strchr( line, '(' );
  if ( epfx == NULL ) {
    epfx = strchr( line, '\0' );
  }
  INT nchr = epfx - line;
  INT lo   = 0;
  INT hi   = N_EVENT - 1;
  INT mid;
  INT cmp;

  while ( lo <= hi ) {
    mid = ( lo + hi ) / 2;
    cmp = strncmp( Event_Tbl[ mid ].name, line, nchr );
    if ( cmp == 0 ) break;
    if ( cmp < 0 ) {
      lo = mid + 1;
    } else {
      hi = mid - 1;
    } 
  }
  if ( cmp != 0 ) {
#ifndef REPORT_FONT_WARNINGS
    if ( strncmp(line, "Font ", 5) == 0 ) {
      return false;
    }
#endif
    fprintf(stderr, "DaVinci::Parse_Event UNKNOWN: %s\n", line);
    return false;
  }
  event->kind = Event_Tbl[ mid ].kind;

  switch ( event->kind ) {
  case EK_COM_ERROR:
    event->u.com_error.msg = line;  // more: better storage management.
    break;
  case EK_QUIT:
  case EK_OK:
  case EK_CLOSE:
  case EK_DISCONNECT:
    // no add'l data.
    break;
  case EK_SEL_EDGE:
    if ( ! Parse_Edge_Id( epfx, event ) ) {
      return false;
    }
    break;
  case EK_SEL_MENU:
    {
      const char *ss = Parse_Menu_Label( epfx );
      if ( ss == NULL ) return false;
      event->u.sel_menu.label = ss;
    }
    break;
  case EK_SEL_NODES:
    if ( ! Parse_Node_Ids( epfx, &event->u.sel_nodes.n_nodes,
			         &event->u.sel_nodes.node_ids ) ) {
      return false;
    }
    break;
  default:
    fprintf(stderr, "INTERNAL ERROR: missing event case %d\n", event->kind);
    return false;
  }
  return true;
}

void
DaVinci::Emit_Menu(INT n_items, const MENU_INFO *items)
{
  for (INT i = 0; i < n_items; ++i) {
    if ( items[i].subitems && items[i].n_subitems > 0 ) {
      _io.Out_Fmt( "submenu_entry(\"%s\", \"%s\",[",
		   items[i].id, items[i].label);
      Emit_Menu( items[i].n_subitems, items[i].subitems );
      _io.Out_Fmt( "])" );
    } else {
      _io.Out_Fmt( "menu_entry(\"%s\", \"%s\")",
		   items[i].id, items[i].label);
    }
    if ( i < n_items - 1 ) _io.Out_Fmt( "," );

    _menu_state.Set( items[i].id,
		     (items[i].initially_active ? DM_ACTIVE : DM_INACTIVE));
  }
}

void
DaVinci::Emit_Attr(const NODE_TYPE& nt, const char **comma)
{
  const char *val = NULL;

  if ( nt._node_color[0] != '\0' ) {
    _io.Out_Fmt( ",a(\"COLOR\",\"%s\")", nt._node_color);
  }

  switch ( nt._node_shape ) {
  case NS_UNSET:    val = NULL;      break;
  case NS_BOX:      val = "box";     break;
  case NS_CIRCLE:   val = "circle";  break;
  case NS_ELLIPSE:  val = "ellipse"; break;
  case NS_RHOMBUS:  val = "rhombus"; break;
  case NS_TEXT:     val = "text";    break;
  default:
    fprintf(stderr, "DaVinci::emit_attr/node unexpected shape %d\n",
	    nt._node_shape);
  }
  if ( val) {
    _io.Out_Fmt( "%sa(\"_GO\",\"%s\")", *comma, val);
    *comma = ",";
  }

  switch ( nt._border ) {
  case NB_UNSET:  val = NULL;      break;
  case NB_SINGLE: val = "single";  break;
  case NB_DOUBLE: val = "double";  break;
  default:
    fprintf(stderr, "DaVinci:emit_attr/node unexpected border type %d\n",
	    nt._border);
  }
  if ( val ) {
    _io.Out_Fmt( "%sa(\"BORDER\",\"%s\")", *comma, val);
    *comma = ",";
  }

  switch ( nt._hide ) {
  case NH_UNSET: val = NULL;     break;
  case NH_HIDE:  val = "true";   break;
  case NH_SHOW:  val = "false";  break;
  default:
    fprintf(stderr, "DaVinci:emit_attr/node unexpected hide/show value %d\n",
	    nt._hide);
  }
  if ( val ) {
    _io.Out_Fmt( "%sa(\"HIDDEN\",\"%s\")", *comma, val);
    *comma = ",";
  }
}

void
DaVinci::Emit_Attr(const EDGE_TYPE& et)
{

  const char *val   = NULL;
  const char *comma = "";

  if ( et._edge_color[0] != '\0' ) {
    _io.Out_Fmt( "a(\"EDGECOLOR\",\"%s\")", et._edge_color );
    comma = ",";
  }

  switch ( et._edge_pattern ) {
  case EP_UNSET:   val = NULL;      break;
  case EP_SOLID:   val = "solid";   break;
  case EP_DOTTED:  val = "dotted";  break;
  case EP_DASHED:  val = "dashed";  break;
  case EP_THICK:   val = "thick";   break;
  case EP_DOUBLE:  val = "double";  break;
  default:
    fprintf(stderr, "DaVinci::emit_attr/edge unexpected edge pattern %d\n",
	    et._edge_pattern);
  }
  if ( val ) {
    _io.Out_Fmt( "%sa(\"EDGEPATTERN\",\"%s\")", comma, val );
    comma = ",";
  }

  switch ( et._edge_dir ) {
  case ED_UNSET:    val = NULL;       break;
  case ED_NORMAL:   val = "normal";   break;
  case ED_INVERSE:  val = "inverse";  break;
  case ED_BOTH:     val = "both";     break;
  case ED_NONE:     val = "none";     break;
  }
  if ( val ) {
    _io.Out_Fmt( "%sa(\"_DIR\",\"%s\")", comma, val );
    comma = ",";
  }
}

void
DaVinci::Menu_Basic_Do( const char *label )
{
  if ( strcmp( label, "exit_event_loop" ) == 0 ) {
    if(_use_socket){
      char s[100];
      sprintf(s, "multi(set_context(\"%d\"))", _contex);
      Emit_Do(s);
      _io.Out_Fmt( "menu(file(close))");
      _io.Out_Fmt( "\n" );
    }else{
      Exit_Event_Loop();
    }
  }
}

DA_ACK
DaVinci::Menu_Set_Active()
{
  bool first = true;

  _io.Out_Fmt( "app_menu(activate_menus([" );
  for (Item_info::iterator m_iter = _menu_state.items.begin();
       m_iter != _menu_state.items.end(); ++m_iter) {
    if ( (*m_iter).second == DM_ACTIVE) {
      const char *menu_id = (*m_iter).first;
      _io.Out_Fmt( "%s\"%s\"", (first ? "" : ","), menu_id );
      first = false;
    }
  }
  _io.Out_Fmt( "]))\n" );

  return Wait_For_Ack();
}

void
DaVinci::Kill_Davinci()
{
    INT stat;
    
    _display_ok = false;
#ifndef __MINGW32__
    kill (_pid, SIGINT);
    waitpid (_pid, &stat, WNOHANG);  // capture any SIGCHLD so not to
				    // confuse master.
#endif /* __MINGW32__ */
    _io.Close();
}

// ------------------------------------------------------------------------
//         DaVinci public members.
// ------------------------------------------------------------------------
DaVinci::DaVinci(MEM_POOL *m, FILE *_trace_fp, bool usage_check) :
  _menu_state(m)
{
#ifndef __MINGW32__
  _m                = m;
  _basic_menu_added = false;
  _in_event_loop    = false;
  _display_ok       = false;
  _usage_check      = usage_check;  // debugging aid, off by default.
  _ftag_last        = FT_DAVINCI;
  _node_cnt         = 0;
  _edge_cnt         = 0;
  FILE *from_display; 
  FILE *to_display;   

  if(_use_socket){
    _davinci_count++;
    
    if(_davinci_count == 1){
      for(int i =0; i  < MAX_VIEW_NUM; i++) {
        _contex_use_array[i] = CONTEX_UNUSE;
      }
      struct sockaddr_in addr_server;
      /* Create a TCP/IP socket. */
      _tcp_socket = socket(PF_INET, SOCK_STREAM, 0);
      if (_tcp_socket < 0) {
        perror( "DaVinci" );
        return;
      }
      /* Create address for uDraw(Graph) server. */
      /* Family for TCP/IP is "Internet". */
      addr_server.sin_family = AF_INET;
      /* IANA registered port for uDraw(Graph). The following two
        values have to be converted from host byte order to network byte
        order. */
      addr_server.sin_port = htons(2542);
      /* Use IP address of localhost assuming uDraw(Graph) is running locally. */
      /* TODO: This has to be extend to show an example for remote
        communication too. */
      FILE *ip_fp;
      char ip_address[100];
      if(!(ip_fp = fopen(getenv("UDRAWIP"), "r"))){
        fprintf(stderr, "Can't get the ip address of DaVinci server!\n");
        perror( "DaVinci" );
        return;
      }
      fscanf(ip_fp, "%s", ip_address);
      addr_server.sin_addr.s_addr = inet_addr(ip_address);
      bzero(&(addr_server.sin_zero),8);
 
      if (connect(_tcp_socket, (struct sockaddr *)&addr_server, sizeof(addr_server)) < 0)
      {
        fprintf(stderr, "Can't connect to  %s:2542, please start using this commandline:\n"
                "uDrawGraph -server\n", ip_address);
        perror( "DaVinci" );
        return;
      }
      
    }
    from_display = fdopen(_tcp_socket,  "r");
    to_display   = fdopen(_tcp_socket, "w");
    char *tracefile   = getenv("UDRAW_TRACEFILE");
 
    setbuf(from_display, NULL);  // more: line buffer better ?
    setbuf(to_display,   NULL);
    _io.Init( to_display, from_display );
    if(_trace_fp == NULL && tracefile){
      _trace_fp = fopen(tracefile, "a");
    }    
    _io.Trace( _trace_fp );
    if(_davinci_count == 1){
      DA_ACK msg = Wait_For_Ack();
      if ( msg ) {
        fprintf(stderr, "DaVinci connection failed: %s\n", msg);
        return;
      }
    }

    _display_ok = true;
    //build a new view
    INT i;
    for( i = 0; i < MAX_VIEW_NUM; i++){
      if(_contex_use_array[i] == CONTEX_UNUSE){
        char s[100];
        sprintf(s, "multi(open_context(\"%d\"))", i);
        Emit_Do( s);
        _contex_use_array[i] = CONTEX_ACTIVE;
        _contex_count++;
        _contex = i;
        break;
      }
    }
    if(i == MAX_VIEW_NUM) {
      fprintf(stderr, "DaVinci:reach the max view size");
    }
  }else{
    INT read_pipe[2];
    INT write_pipe[2];
 
    if ( pipe( read_pipe ) == -1 || pipe(write_pipe) == -1 ) {
      perror( "DaVinci" );
      return;
    }
    from_display = fdopen(read_pipe[0],  "r");
    to_display   = fdopen(write_pipe[1], "w");
    char *logfile      = getenv("DAVINCI_LOGFILE");
 
    setbuf(from_display, NULL);  // more: line buffer better ?
    setbuf(to_display,   NULL);
 
    switch ( _pid = fork() ) {
    case -1:		       // can't fork
      fprintf(stderr, "Unable to fork (for daVinci)\n");
      close (read_pipe[0]);
      close (read_pipe[1]);
      close (write_pipe[0]);
      close (write_pipe[1]);
      return;
    case 0:			// child
      dup2 (write_pipe[0], 0);    // reset stdin, stdout, and stderr
      dup2 (read_pipe[1], 1);
      dup2 (read_pipe[1], 2);
      
      close (write_pipe[0]);	// close unused pipes.
      close (read_pipe[1]);	// leave the other pair opened so that
          			// daVinci does not die when master exits
      if ( logfile ) {
        char fname[1000];
        // append time to avoid overwriting previous log file,
        // which would happen if daVinci is started more than
        // once in a session.
        sprintf(fname, "%s.%ld", logfile, time(NULL));
        execlp ("daVinci", "daVinci", "-pipe", "-log", fname, NULL);
      } else {
        execlp ("daVinci", "daVinci", "-pipe", NULL);
      }
      // error to stdout so it appears on pipe read by parent proc.
      // use message syntax that DaVinci::Parse_Event() recognizes.
      //   more? detected if errno out of range for sys_errlist.
      printf("communication_error(\"execlp of daVinci: %s %s\")\n",
             strerror(errno), "(define $DAVINCIHOME; need daVinci on $PATH)");
      exit (1);
 
    default:	                // parent
      close (read_pipe[1]);
      close (write_pipe[0]);
    }
    _io.Init( to_display, from_display );
    _io.Trace( _trace_fp );
 
    DA_ACK msg = Wait_For_Ack();
    if ( msg ) {
      fprintf(stderr, "DaVinci connection failed: %s\n", msg);
      return;
    }
    _display_ok = true;
  }
  
#ifdef TARG_IA64
  Emit_Do( "set(font_size(12))" );  // more? provide external control.
  Emit_Do( "set(gap_height(10))" );
  Emit_Do( "set(gap_width(10))" );
#else
  Emit_Do( "set(font_size(6))" );  // more? provide external control.
  Emit_Do( "set(gap_height(40))" );
  Emit_Do( "set(gap_width(20))" );
#endif
#endif /* __MINGW32__ */
}

DaVinci::~DaVinci()
{
  if(_use_socket){
    _davinci_count--;
    if(_davinci_count ==0) close(_tcp_socket);
  }
  // more? wish to allow DaVinci to stay up.  is it possible close pipes.
}

// -- Limit Menu_basic to one level -- see menu_basic_do().
//    The list is expected to be very small.
//
static MENU_INFO Menu_basic[] = {
  { "exit_event_loop", "exit_event_loop", true, 0, NULL }
};
#define N_MENU_BASIC  ( sizeof(Menu_basic) / sizeof(Menu_basic[0]) )

void
DaVinci::Event_Loop(DaVinci_Callback *cb_hook)
{
  static DaVinci_Callback dflt_cb_hook;

  EVENT_T event;
  INT     i;

  if ( _in_event_loop || ! _display_ok ) return;

  if ( cb_hook == NULL ) {
    cb_hook = &dflt_cb_hook;
  }
  
  if ( ! _basic_menu_added ) {
    DA_ACK msg = Menu_Create( N_MENU_BASIC, Menu_basic );
    if ( msg ) {
      fprintf(stderr, "Unable to add Basic Menu -- %s.\n",
	      "best to not start event_loop");
      return;
    }
    _basic_menu_added = true;
  }
  _in_event_loop = true;

  while ( _display_ok ) {
    while ( (_use_socket && !_event_q_socket[_contex].empty()) ||
            (!_use_socket && !_event_q.empty())) {
      if(_use_socket){
        event = _event_q_socket[_contex].front();
        _event_q_socket[_contex].pop();
      }else{
        event = _event_q.front();
        _event_q.pop();
      }
      switch ( event.kind ) {
      case EK_COM_ERROR:
	fprintf(stderr, "event_loop: Unexpected: %s\n", event.u.com_error.msg);
	break;
      case EK_OK:
	fprintf(stderr, "event_loop: Unexpected: OK\n");
	break;
      case EK_SEL_EDGE:
	{
	  EDGE_ID edge_id(event.u.sel_edge.edge_src,
			  event.u.sel_edge.edge_dst);
	  cb_hook->Edge_Select( edge_id );
	}
	break;
      case EK_SEL_MENU:
	for (i = 0; i < N_MENU_BASIC; ++i) {
	  if ( strcmp( event.u.sel_menu.label, Menu_basic[i].id ) == 0 ) {
	    Menu_Basic_Do( event.u.sel_menu.label );
	    break;
	  }
	}
	if ( i >= N_MENU_BASIC ) {
	  cb_hook->Menu_Select( event.u.sel_menu.label );
	}
	break;
      case EK_SEL_NODES:
	cb_hook->Node_Select( event.u.sel_nodes.n_nodes,
			      event.u.sel_nodes.node_ids );
	break;
      case EK_QUIT:
      case EK_CLOSE:
        if(_use_socket){
  	   _contex_use_array[_contex] = CONTEX_UNUSE;
          _contex_count--;
          if(_contex_count == 0){
            close(_tcp_socket);
          }
    	   _display_ok    = false;  // DaVinci exited.
    	   _in_event_loop = false;
        }else{
          _display_ok    = false;  // DaVinci exited.
          _in_event_loop = false;
        }
        break;
      case EK_DISCONNECT:
        fprintf(stderr, "EVENT: EK_DISCONNET\n");
	break;
      default:
	fprintf(stderr, "ERROR: event_loop missing event case %d\n",
		event.kind);
      }
      if ( ! _in_event_loop ) {
	return;
      }
    }

    
    char *line = _io.In_Line();

    if ( line == NULL ) {
      _display_ok = false;
      break;
    }
    
    if(_use_socket && line[0] == 'c' && line[1] == 'o' && line[2] == 'n' 
      && line[3] == 't' && line[4] == 'e' && line[5] == 'x' && line[6] == 't'){
      if(line[9] >='0' && line[9] <= '9'){
        sscanf(line, "context(\"%d\")", &_current_contex);
      }
      continue;
    }
    if ( Parse_Event( line, &event ) ) {
      if(_use_socket){
        if(event.kind == EK_QUIT  || event.kind ==EK_CLOSE){
          while ( ! _event_q_socket[_current_contex].empty() ) {
            _event_q_socket[_current_contex].pop();
          }
        }
        _event_q_socket[_current_contex].push( event );
      }else{
        _event_q.push( event );
      }
    }
  }
  // more? flush event queue ? -- might reenter event loop later ..
}

void
DaVinci::Exit_Event_Loop()
{
  _in_event_loop = false;  // always to do this.
}

DA_ACK
DaVinci::Title(const char *title)
{
  if ( ! Usage_Ok( FT_TITLE, BASE_SET ) ) return "Usage-error";

  _io.Out_Fmt( "window(title(\"%s\"))\n", title );
  return Wait_For_Ack();
}

DA_ACK
DaVinci::Show_Status(const char *status)
{
  if ( ! Usage_Ok( FT_SHOW_STATUS, BASE_SET ) ) return "Usage-error";

  _io.Out_Fmt( "window(show_status(\"%s\"))\n", status );
  return Wait_For_Ack();
}

DA_ACK
DaVinci::Show_Message(const char *msg)
{
  if ( ! Usage_Ok( FT_SHOW_MESSAGE, BASE_SET ) ) return "Usage-error";

  _io.Out_Fmt( "window(show_message(\"%s\"))\n", msg );
  return Wait_For_Ack();
}

DA_ACK
DaVinci::Menu_Create(INT n_items, const MENU_INFO *items)
{
  if ( ! Usage_Ok( FT_MENU_CREATE, BASE_SET ) ) return "Usage-error";

  if ( n_items == 0 ) return NULL;  // no-op.

  _io.Out_Fmt( "app_menu(create_menus([" );
  Emit_Menu( n_items, items );
  _io.Out_Fmt( "]))\n" );

  DA_ACK msg = Wait_For_Ack();
  if ( msg ) return msg;

  return Menu_Set_Active();
}

DA_ACK
DaVinci::Menu_Activate(INT n_ids, const char *id[])
{
  if ( ! Usage_Ok( FT_MENU_ACTIVATE, BASE_SET ) ) return "Usage-error";

  for (INT i = 0; i < n_ids; ++i) {
    _menu_state.Set( id[i], DM_ACTIVE );
  }
  return Menu_Set_Active();
}

DA_ACK
DaVinci::Menu_Deactivate(INT n_ids, const char *id[])
{
  if ( ! Usage_Ok( FT_MENU_DEACTIVATE, BASE_SET ) ) return "Usage-error";

  for (INT i = 0; i < n_ids; ++i) {
    _menu_state.Set( id[i], DM_INACTIVE );
  }
  return Menu_Set_Active();
}

void
DaVinci::Graph_Begin()
{
  if ( ! Usage_Ok( FT_GRAPH_BEGIN, BASE_SET ) ) return;

  _io.Out_Fmt( "graph(new_placed([" );
  _node_cnt = 0;
}

void
DaVinci::Node_Begin(NODE_ID id, const char *label, const NODE_TYPE& node_type)
{
  if ( ! Usage_Ok( FT_NODE_BEGIN, (FT_GRAPH_BEGIN|FT_NODE_END) ) ) return;

  if ( _usage_check ) {
    if ( _node_def_set.count(id) > 0 ) {
      fprintf(stderr, "DaVinci::Node_Begin USAGE-ERROR, %s 0x%p\n",
	      "duplicate def for node", id);
    } else {
      _node_def_set.insert(id);
    }
  }
  _io.Out_Fmt( "%sl(\"%p\",n(\"%s\",[a(\"OBJECT\",\"%s\")",
	       ( _node_cnt > 0 ? "," : "" ),
	       id, node_type._type_name, label);
  _node_cnt += 1;
  _edge_cnt =  0;           // i.e., out edges for this node decl.
  const char *comma = ",";
  Emit_Attr( node_type, &comma );
  _io.Out_Fmt( "],[" );     // end node attributes, begin out_edges.

}

void
DaVinci::Out_Edge(const EDGE_ID&   edge_id,
		  const EDGE_TYPE& edge_type,
		  const NODE_ID    dest_id)
{
  if ( ! Usage_Ok( FT_OUT_EDGE, (FT_NODE_BEGIN|FT_OUT_EDGE) ) ) return;

  if ( _usage_check ) {
    _node_ref_set.insert(edge_id.dst);
  }
  _io.Out_Fmt( "%sl(\"%p:%p\",e(\"%s\",[",
	       ( _edge_cnt > 0 ? "," : "" ),
	       edge_id.src, edge_id.dst, edge_type._type_name);
  _edge_cnt += 1;

  Emit_Attr( edge_type );
  _io.Out_Fmt( "],r(\"%p\")))", dest_id);
}

void
DaVinci::Node_End()
{
  if ( ! Usage_Ok( FT_NODE_END, (FT_NODE_BEGIN|FT_OUT_EDGE) ) ) return;

  _io.Out_Fmt( "]))" );
}

DA_ACK
DaVinci::Graph_End()
{
  if ( _usage_check ) {
    for (std::set<NODE_ID>::iterator it_ref = _node_ref_set.begin();
	 it_ref != _node_ref_set.end(); ++it_ref) {
      NODE_ID ref_id = *it_ref;

      if ( _node_def_set.count(ref_id) == 0 ) {
	fprintf(stderr, "ERROR DaVinci node 0x%p referenced, %s\n",
		ref_id, "but not defined.");
      }
    }
  }
  if ( ! Usage_Ok( FT_GRAPH_END, (FT_NODE_END|FT_GRAPH_BEGIN) ) ) {
    return "Usage-error";
  }
  _io.Out_Fmt( "]))\n" );

  return Wait_For_Ack();
}

DA_ACK
DaVinci::Change_Attr(const NODE_ID     id,
		     const NODE_TYPE&  nt,
		     const char       *new_label)
{
  if ( ! Usage_Ok( FT_CHANGE_ATTR, BASE_SET ) ) return "Usage-error";

  const char *comma = "";

  _io.Out_Fmt( "graph(change_attr([node(\"%p\",[", id);

  if ( new_label ) {
    _io.Out_Fmt( "a(\"OBJECT\",\"%s\")", new_label );
    comma = ",";
  }
  Emit_Attr( nt, &comma );

  _io.Out_Fmt( "])]))\n");

  return Wait_For_Ack();
}

DA_ACK
DaVinci::Change_Attr(const EDGE_ID& edge_id, const EDGE_TYPE& et)
{
  if ( ! Usage_Ok( FT_CHANGE_ATTR, BASE_SET ) ) return "Usage-error";

  _io.Out_Fmt( "graph(change_attr([edge(\"%p:%p\",[",
	       edge_id.src, edge_id.dst );
  Emit_Attr( et );
  _io.Out_Fmt( "])]))\n");

  return Wait_For_Ack();
}

void
DaVinci::Update_Begin()
{
  if ( ! Usage_Ok( FT_UPDATE_BEGIN, BASE_SET ) ) return;

  _io.Out_Fmt( "graph(update([" );
  _node_cnt = 0;
  _edge_cnt = 0;
}

void
DaVinci::New_Node(NODE_ID id, const char *label, const NODE_TYPE& nt )
{
  if ( ! Usage_Ok( FT_NEW_NODE, (FT_UPDATE_BEGIN|FT_NEW_NODE) ) ) return;

  if ( _edge_cnt > 0 ) {
    fprintf(stderr, "Must list ALL new_nodes before first new_edge\n");
    fprintf(stderr, "Skipping this node to avoid DaVinci error.\n");
    return;
  }
  _io.Out_Fmt( "%snew_node(\"%p\",[a(\"OBJECT\",\"%s\")",
	       (_node_cnt > 0 ? "," : ""), id, label );
  const char *comma = ",";
  Emit_Attr( nt, &comma );
  _node_cnt += 1;
}

void
DaVinci::New_Edge(const EDGE_ID&   id,
		  const EDGE_TYPE& et,
		  NODE_ID          src,
		  NODE_ID          dst)
{
  if ( ! Usage_Ok( FT_NEW_EDGE, (FT_UPDATE_BEGIN|FT_NEW_NODE|FT_NEW_EDGE) ) ) {
    return;
  }
  if ( _edge_cnt == 0 ) {
    _io.Out_Fmt( "],[" );  // end new_node + begin new_edge list.
  }
  _io.Out_Fmt( "%snew_edge(\"%p:%p\",\"\",[", (_edge_cnt > 0 ? "," : ""),
	       id.src, id.dst );
  Emit_Attr( et );
  _io.Out_Fmt( "],\"%p\",\"%p\")", src, dst);
  _edge_cnt += 1;
}

void
DaVinci::Delete_Edge(const EDGE_ID&   id)
{
  if ( ! Usage_Ok( FT_DELETE_EDGE,
         (FT_UPDATE_BEGIN|FT_NEW_NODE|FT_NEW_EDGE|FT_DELETE_EDGE) ) ) {
    return;
  }
  if ( _edge_cnt == 0 ) {
    _io.Out_Fmt( "],[" );  // end new_node + begin new_edge list.
  }
  _io.Out_Fmt( "%sdelete_edge(\"%p:%p\")", (_edge_cnt > 0 ? "," : ""),
	       id.src, id.dst );
  _edge_cnt += 1;
}

DA_ACK
DaVinci::Update_End()
{
  if ( ! Usage_Ok( FT_UPDATE_END,
		   (FT_UPDATE_BEGIN|FT_NEW_NODE|FT_NEW_EDGE) ) ) {
    return "Usage-error";
  }
  if ( _edge_cnt == 0 ) {
    _io.Out_Fmt( "],[" );  // end new_node + begin empty new_edge list.
  }
  _io.Out_Fmt( "]))\n");

  return Wait_For_Ack();
}
