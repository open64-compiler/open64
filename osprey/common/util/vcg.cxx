#include <errno.h>
#include <stdlib.h>
#include <malloc.h>
#include <stdio.h>
#include <string.h>
#include "vcg.h"
#include "errors.h"


VCGGraph::VCGGraph(const char *title, 
		  VCGLayoutAlgorithm layout, 
		  VCGColors color, 
		  UInt32 width, 
		  UInt32 height)
   : theTitle(title), 
     theLayout(layout), 
     theColor(color), 
     theNodes(NULL), 
     theEdges(NULL), 
     theClassNames(NULL), 
     theInfo1Name(NULL), 
     theInfo2Name(NULL), 
     theInfo3Name(NULL), 
     theTextColor(Magenta),
     theShape(Ellipse), 
     info1(NULL),
     info2(NULL),
     info3(NULL),
     theWidth(width), 
     theHeight(height),
     theNumberOfEdgeClasses(0),
     theSubGraphs(NULL),
     theNext(NULL),
     nearEdges(FALSE), 
     theXspace(20), 
     theYspace(25), 
     doFineTuning(FALSE),
     theDisplayEdgeLabels(TRUE),
     thePortSharing(TRUE),
     thePriorityPhase(FALSE),
     theSplines(FALSE),
     theFolding(FALSE),
     isSubgraph(FALSE),
     theNodeCount(0),
     theEdgeCount(0)
{
}

VCGGraph::~VCGGraph()
{
}

const char * colorMnemonic[VCGColorsEnumCount] = 
{
   "black", 
   "blue", 
   "red", 
   "green", 
   "yellow", 
   "magenta", 
   "cyan", 
   "white",
   "darkgrey", 
   "darkblue", 
   "darkred", 
   "darkgreen",
   "darkyellow", 
   "darkmagenta", 
   "darkcyan", 
   "gold", 
   "lightgrey", 
   "lightblue", 
   "lightred", 
   "lightgreen", 
   "lightyellow", 
   "lightmagenta", 
   "lightcyan", 
   "lilac", 
   "turquoise", 
   "aquamarine", 
   "khaki", 
   "purple",
   "yellowgreen", 
   "pink", 
   "orange"
};

const char *layoutMnemonic[VCGLayoutAlgorithmEnumCount] = 
{
   "",

   "maxdepth",
   "mindepth", 
   "maxdepthslow",
   "mindepthslow",
 
   "maxdegree",
   "mindegree",
 
   "maxindegree",
   "minindegree",
 
   "maxoutdegree",
   "minoutdegree",
 
   "minbackward",
   "dfs",
   "tree"
};

const char * lineStyleMnemonic[VCGEdgeLineStyleCount] =
{
   "",
   "continuous",
   "dashed",
   "dotted",
   "invisible"
};

const char *shapeMnemonic[NodeShapeEnumCount] = {"box", "rhomb", "ellipse",
					   "triangle" }; 

void
VCGGraph::emit(const char *fname) const
{
    FmtAssert(fname != NULL,("fname must be non-null"));

    FILE *f = fopen( fname, "w" );
    if (!f) {
        fprintf( stderr, "Cannot open VCG output file: %s - %s\n",
                 fname, strerror(errno) );
        return;
    }
    emit(f);
    fclose(f);
}

void
VCGGraph::emit(FILE *out) const
{
   fprintf(out, "graph:\n"); 
   fprintf(out, "{\n"); 
   fprintf(out, "title: \"%s\" \n", theTitle); 
   fprintf(out, "color: %s \n", colorMnemonic[theColor]); 
   if (info1 != NULL)
      fprintf(out, "info1: \"%s\" \n", info1); 
   if (info2 != NULL)
      fprintf(out, "info2: \"%s\" \n", info2); 
   if (info3 != NULL)
      fprintf(out, "info3: \"%s\" \n", info3);
   if (width() != NoWidth)
       fprintf(out, "width: %d \n", width());
   if (height() != NoHeight)
       fprintf(out, "height: %d \n", height());
   if (isSubgraph) {
      // For a subgraph, color and shape refer to the summary node.
      // For a non-subgraph, these would be safe, but are unneeded.
      fprintf(out, "textcolor: %s \n", colorMnemonic[theTextColor]);
      fprintf(out, "shape: %s \n", shapeMnemonic[theShape]);
      // For the sub graph, setting the status to grey keeps it
      // folded by default. 
 
      if ((theNodeCount >= 25)
          || (theEdgeCount >= 25))
      {
        // keep the subgraph folded
        fprintf(out, "status: grey\n");
      }
   }
   else { // non-subgraph
      // xvcg currently does not accept any of the following
      // attributes for a subgraph.
      if (theLayout != UnspecifiedLayout) {
	 fprintf(out, 
		 "layoutalgorithm: %s \n", layoutMnemonic[theLayout]); 
      }
      fprintf(out, "finetuning: %s \n", fineTuning() ? "yes" : "no"); 
      fprintf(out, "xspace: %d\n", xspace()); 
      fprintf(out, "yspace: %d\n", yspace()); 
      fprintf(out, "nearedges: %s \n", allowNearEdges() ? "yes" : "no"); 
      fprintf(out, "splines: %s \n", splines() ? "yes" : "no");
      fprintf(out, "port_sharing: %s \n", portSharing() ? "yes" : "no");
      fprintf(out, "priority_phase: %s \n", priorityPhase() ? "yes" : "no");
      if (displayEdgeLabels())
      {
	 fprintf(out, "display_edge_labels: yes\n"); 
	 if (displayEdgeLabelsLate())
	    fprintf(out, "late_edge_labels: yes\n"); 
      }
   }

   const char *infoNameStr = NULL; 
   if ((infoNameStr = infoName(1)) != NULL)
      fprintf(out, "infoname 1: \"%s\" \n", infoNameStr); 

   if ((infoNameStr = infoName(2)) != NULL)
      fprintf(out, "infoname 2: \"%s\" \n", infoNameStr); 

   if ((infoNameStr = infoName(3)) != NULL)
      fprintf(out, "infoname 3: \"%s\" \n", infoNameStr); 

   for (VCGNode *nodes = theNodes; nodes != NULL; nodes = nodes->next())
   {
      nodes->emit(out); 
   }

   // Emit all the subgraphs. 
   for (VCGGraph *aSubGraph = theSubGraphs; 
	aSubGraph != NULL; 
	aSubGraph = aSubGraph->theNext)
   {
      aSubGraph->emit(out); 
   }

   // Emit all the edge class names. 
   UInt32 i = theNumberOfEdgeClasses; 
   for (className *aClass = theClassNames; 
	aClass != NULL; 
	aClass = aClass->theNext, i--)
   {
      fprintf(out, "classname %d: \"%s\"\n", i, aClass->theName); 
      if (aClass->isHidden) 
	 fprintf(out, "hidden: %d\n", i); 
   }

   // We will make three passes over the edges.  All the back edges
   // will be emitted first, and then the normal edges.  This allows
   // VCG to render the layout better.  Furthermore, there appears to
   // be an xvcg bug such that if there are multiple backedges with
   // the same source and target, only the label (if any) on the first
   // is considered; by emitting all labeled backedges before any
   // unlabeled backedge, we prevent an unlabeled backedge from hiding
   // the label of a labeled backedge, although it's still possible
   // for one labeled backedge to hide the label of another labeled
   // backedge.
   VCGEdge *edges;
   for (edges = theEdges; edges != NULL; edges = edges->next())
   {
      if (edges->backEdge() && (edges->label() != NULL))
	 edges->emit(out); 
   }
   for (edges = theEdges; edges != NULL; edges = edges->next())
   {
      if (edges->backEdge() && (edges->label() == NULL))
	 edges->emit(out); 
   }

   // Now for the forward edges. 
   for (edges = theEdges; edges != NULL; edges = edges->next())
   {
      if (!edges->backEdge())
	 edges->emit(out); 
   }

   fprintf(out, "}\n"); 

}

void
VCGGraph::addNode(VCGNode &aNode)
{
   aNode.next(theNodes); 
   theNodes = &aNode; 
   theNodeCount++;
}

void
VCGGraph::addEdge(VCGEdge &anEdge)
{
   anEdge.next(theEdges); 
   theEdges = &anEdge; 
   theEdgeCount++;
   if (anEdge.label() != NULL)
      displayEdgeLabels(TRUE); 
}

void 
VCGGraph::addSubGraph(VCGGraph &aSubGraph) 
{ 
   aSubGraph.next(theSubGraphs); 
   theSubGraphs = &aSubGraph; 
   aSubGraph.isSubgraph = TRUE;
}

UInt32 
VCGGraph::edgeClass (const char *name, Boolean hidden)
{
   struct className *aNewName = new className(name, hidden); 
   if (theClassNames == (struct className *) NULL)
      theClassNames = aNewName; 
   else
   {
      aNewName->theNext = theClassNames; 
      theClassNames = aNewName; 
   }
   return ++theNumberOfEdgeClasses; 
}


const char * 
VCGGraph::infoName(int infoId) const
{
   const char *result = NULL; 

   switch (infoId)
   {
   case 1: 
      result = theInfo1Name; 
      break; 
   case 2: 
      result = theInfo2Name; 
      break; 
   case 3: 
      result = theInfo3Name; 
      break; 
   default:
      FmtAssert(FALSE,("Bad info string sent to VCG graph"));
   }

   return result; 
}

void 
VCGGraph::infoName(UInt32 infoId, const char *name)
{
   switch (infoId)
   {
   case 1: 
      theInfo1Name = name; 
      break; 
   case 2: 
      theInfo2Name = name; 
      break; 
   case 3: 
      theInfo3Name = name; 
      break; 
   default:
     FmtAssert(FALSE,("Bad info name sent to VCG graph.\n")); 
   }
}

void
VCGGraph::info(UInt32 infoId, const char *information)
{
   switch (infoId)
   {
   case 1: 
      info1 = information; 
      break; 
   case 2: 
      info2 = information; 
      break; 
   case 3: 
      info3 = information; 
      break; 
   default:
     FmtAssert(FALSE,("Bad info string sent to VCG graph"));
   };
}

void
VCGNode::emit(FILE *out) const
{
   fprintf(out, "node:\n"); 
   fprintf(out, "{\n"); 
   fprintf(out, "title: \"%s\" \n", theTitle); 
   if (theLabel != NULL)
      fprintf(out, "label: \"%s\" \n", theLabel); 
   fprintf(out, "color: %s \n", colorMnemonic[backGroundColor()]); 
   fprintf(out, "textcolor: %s \n", colorMnemonic[textColor()]); 
   fprintf(out, "bordercolor: %s \n", colorMnemonic[borderColor()]); 
   fprintf(out, "shape: %s\n", shapeMnemonic[shape()]); 
   if (info1 != NULL)
      fprintf(out, "info1: \"%s\" \n", info1); 
   if (info2 != NULL)
      fprintf(out, "info2: \"%s\" \n", info2); 
   if (info3 != NULL)
      fprintf(out, "info3: \"%s\" \n", info3); 
   fprintf(out, "}\n"); 
}




void
VCGNode::info(UInt32 infoId, const char *information)
{
   switch (infoId)
   {
   case 1: 
      info1 = information; 
      break; 
   case 2: 
      info2 = information; 
      break; 
   case 3: 
      info3 = information; 
      break; 
   default:
      FmtAssert(FALSE,("Bad info string sent to VCG Node"));
   };
}

VCGNode::VCGNode(const char *title, 
		 const char *label, 
		 NodeShape shape)
      : theTitle(title), 
	theLabel(label), 
	theShape(shape), 
	theNext(NULL), 
	info1(NULL),
	info2(NULL),
	info3(NULL), 
	theBackgroundColor(Khaki), 
	theTextColor(Magenta), 
	theBorderColor(Black)
{
}

VCGNode::~VCGNode ()
{
}

VCGEdge::VCGEdge (const char *sourceName, const char *targetName)
      : theSourceName(sourceName), 
	theTargetName(targetName), 
	theNext(NULL), 
	theClass(0),
	theColor(Black),
	theLabel(NULL),
	isBackEdge(FALSE),
	theLineStyle(UnspecifiedLineStyle)
{
}

VCGEdge::~VCGEdge ()
{
}



void
VCGEdge::emit(FILE *out) const
{
   if (backEdge())
      fprintf(out, "backedge:\n"); 
   else
      fprintf(out, "edge:\n"); 
   fprintf(out, "{\n"); 

   fprintf(out, "sourcename: \"%s\" \n", theSourceName); 
   fprintf(out, "targetname: \"%s\" \n", theTargetName); 
   fprintf(out, "class: %d\n", theClass); 
   fprintf(out, "color: %s\n", colorMnemonic[theColor]); 
   if (label() != NULL)
      fprintf(out, "label: \"%s\" \n", label());
   if (lineStyle() != UnspecifiedLineStyle)
      fprintf(out, "linestyle: %s\n", lineStyleMnemonic[lineStyle()]);
   fprintf(out, "}\n");
}

/***********************************************************************
* 
*   PROCEDURE:	VCGGraph::dump
* 
*   INPUTS:     VCG graph to dump, filename, whether to display now
* 
*   OUTPUTS:	None
* 
*   SIDEEFFECTS: Dumps the VCG graph to the specified file and optionally
*                displays it using xvcg
* 
*   DESCRIPTION:
*               This is a helper function to dump a VCG graph
* 
***********************************************************************/
void
VCGGraph::dump(const char *file, Boolean displayNow) const
{
   FILE *out = fopen(file, "w");
   if (!out) {
      printf("VCGGraph::dump: could not open \"%s\": %s\n",
	     file, strerror(errno));
      return;
   }
   emit(out);
   fclose(out);

   if (displayNow) {
      const char *xvcg = "xvcg ";
      size_t commandLength = strlen(xvcg) + strlen(file) + 32;
      char *command = (char *) malloc(commandLength);
      if (!command) {
	 printf("VCGGraph::dump: malloc failed\n");
	 return;
      }
      sprintf(command, "%s %s 1>/dev/null 2>&1 &", xvcg, file);
      system(command);
      free(command);
   }
}

