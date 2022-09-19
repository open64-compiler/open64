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


#ifndef motifutil_INCLUDED
#define motifutil_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

/* ====================================================================
 * ====================================================================
 *
 * Module: motifutil.h
 * $Revision: 1.1.1.1 $
 * $Date: 2005/10/21 19:00:00 $
 * $Author: marcel $
 * $Source: /proj/osprey/CVS/open64/osprey1.0/common/util/motifutil.h,v $
 *
 * Revision history:
 *  26-Feb-92 - Original Version
 *
 * Description:
 *
 * Interface to utilities for Motif user interface creation.
 *
 * ====================================================================
 * ====================================================================
 */


#ifdef _KEEP_RCS_ID
static char *motifutil_rcs_id = "$Source: /proj/osprey/CVS/open64/osprey1.0/common/util/motifutil.h,v $ $Revision: 1.1.1.1 $";
#endif /* _KEEP_RCS_ID */

/* Callback for quitting the program: */
extern void Quit_Callback (
  Widget w,			/* Invoking widget */
  XtPointer client_data,	/* Client's data */
  XtPointer call_data		/* Callback data */
);

/* Callback for unmanaging a popup menu: */
extern void Unmanage_Callback (
  Widget w,		/* Invoking widget */
  XtPointer form,	/* Client's data -- form to unmanage */
  XtPointer call_data	/* Callback data */
);

/* Create a radio box widget, with the given parent and Args, and with
 * children which are toggle button gadgets with labels given.
 *
 * If title is non-NULL, the radio box is put in a form with the title
 * above it.  The args passed in are set for the top-level widget
 * created, whether it is a form or the radio box.
 */
extern Widget Build_Radio_Box (
  Widget parent,	/* Parent widget */
  char 	*title,		/* Title or NULL */
  char	*name,		/* Name of radio box */
  Arg	wargs[],	/* Argument array, with some extra space */
  int	nargs,		/* Current size of wargs array */
  char	*labels[],	/* Button label array */
  int	nlabels		/* Number of buttons */
);

/* ====================================================================
 *
 * Pulldown Menus
 *
 * We define below a struct for describing the desired structure of a
 * possibly cascading set of pulldown menu items, and a general routine
 * which creates them based on such structs.  This is based on an
 * article in The X Journal, Jan/Feb 1992, by Dan Heller.
 *
 * ====================================================================
 */

/* Define the menu item structure.  The expected usage will be to
 * declare a static array of these, describing all of the items in a
 * pulldown menu.  For purposes of Build_Pulldown_Menu below, such an
 * array should be terminated by a NULL record.
 */
typedef struct _menu_item {
  char	*label;		/* Item's label */
  WidgetClass *class;	/* Item's class: pushbutton, label, separator */
  char	mnemonic;	/* Item's mnemonic: NULL if none */
  char	*accelerator;	/* Item's accelerator: NULL if none */
  char	*accel_text;	/*   will be converted to compound string */
  void	(*callback)();	/* Callback routine: NULL if none */
  XtPointer callback_data;	/* client_data for callback() */
  struct _menu_item *subitems;	/* Pull-right menu items, or NULL */
} MENU_ITEM;

/* Define the access macros, based on the array of structs model: */
#define	MITEM_label(m,i)	((m[i]).label)
#define	MITEM_class(m,i)	((m[i]).class)
#define	MITEM_mnemonic(m,i)	((m[i]).mnemonic)
#define	MITEM_accelerator(m,i)	((m[i]).accelerator)
#define	MITEM_accel_text(m,i)	((m[i]).accel_text)
#define	MITEM_callback(m,i)	((m[i]).callback)
#define	MITEM_callback_data(m,i) ((m[i]).callback_data)
#define	MITEM_subitems(m,i)	((m[i]).subitems)

/* Define the construction routine.  Pulldown menus are built from
 * cascade buttons, so this function creates the cascade button which
 * owns the menu, the menu, and any submenu specified:
 */
extern Widget Build_Pulldown_Menu (
  Widget parent,
  char	*menu_title,
  char	menu_mnemonic,
  MENU_ITEM *items
);

#ifdef __cplusplus
}
#endif
#endif /* motifutil_INCLUDED */
