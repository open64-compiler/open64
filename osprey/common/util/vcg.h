// Generic vcg graph dumping class 

#ifndef __VCG_H
#define __VCG_H

// #include "langtypes.h"
#include <stdint.h>
#include <stdio.h>
#include "defs.h"
#define UInt32 uint32_t 
#define Boolean bool
// #define FALSE false
// #define TRUE true
#define MAX_UInt_32  0xffffffff

class VCGNode;
class VCGEdge;
class VCGGraph;



////////////////////////////////////////////////////////////////////////////
//
//   This is the specification of an interface to the "VCG" tool. The
//   tool was written by Georg Sander from the Universitat des
//   Saarlandes, Germany (email - sander@cs.uni-sb.de). The package is
//   designed to help in the visualization of compiler graphs (hence
//   the name VCG). 
//
//
//   Features:
//
//   o A graph may have various class of edges. Each class of edge may
//     be "hidden" or visible. Interactively, one may visualise the
//     graph with one or more classes of edges present. 
//
//   o A graph may contain any number of sub-graphs. Sub-graphs are
//     not necessarily laid out immediately, and can be interactively
//     folded and unfolded. This allows a pleasing representaion of
//     complex and large graphs. You may fold and unfold as you need.
//
//   o A node may contain three different kinds of information
//     associated with it. These are not represented in the node of
//     the graph, but can be visualised by using a mouse key. 
//
//   These are the main features, over and above the standard set of
//   features one would expect from any self-respecting graph layout
//   package. 
//
//
////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////
//
// CLASS:        VCGNode
//
// BASE CLASSES: None.
//
// SUBCLASSES:   None.
//
// FRIENDS:      None.
//
// PURPOSE:	
//     To represent a VCG node. 
//
// NOTES:
//
//////////////////////////////////////////////////////////////////////////


// The various shapes of nodes. 
enum NodeShape { Box, 
                 Rhombus,
		 Ellipse, 
		 Triangle,
		 NodeShapeEnumCount}; 

// The various colors for edges, node backgrounds, borders, text etc. 
enum VCGColors { Black, Blue, Red, Green, Yellow, Magenta, Cyan, 
		 White, Darkgrey, Darkblue, DarkRed, DarkGreen,
		 DarkYellow, DarkMagenta, DarkCyan, Gold, LightGrey, 
		 LightBlue, LightRed, LightGreen, LightYellow, LightMagenta, 
		 LightCyan, Lilac, Turquoise, Aquamarine, Khaki, Purple,
		 YellowGreen, Pink, Orange,
                 VCGColorsEnumCount }; 

// The various layout hints. 
enum VCGLayoutAlgorithm { UnspecifiedLayout,
                          MaxDepth, MinDepth, 
			  MaxDepthSlow, MinDepthSlow, 
			  MaxDegree, MinDegree, 
			  MaxInDegree, MinInDegree, 
			  MaxOutDegree, MinOutDegree, 
			  MinBackward, 
			  Dfs, Tree, VCGLayoutAlgorithmEnumCount}; 

// The various line styles for edges.
enum VCGEdgeLineStyle { UnspecifiedLineStyle,
			Continuous, Dashed, Dotted, Invisible,
                        VCGEdgeLineStyleCount };

class VCGGraph;
class VCGEdge;

class VCGNode
{
	friend class VCGGraph;

public:

	VCGNode(const char *title,
		const char *label = NULL, NodeShape shape = Box);
	~VCGNode();

	// Attach an information string to the node.
	void info(UInt32 infoId, const char *information);

	// Emit the GDL to the specified file. The GDL is appended to the
	// existing file. The file is assumed ready to be written to (i.e
	// it has been opened).
	void emit(FILE *) const;

	const char *title() const { return theTitle; }
	void title(const char *val) { theTitle = val; }

	const char *label() const { return theLabel; }
	void label(const char *val) { theLabel = val; }

	void backGroundColor (VCGColors aColor)
	    { theBackgroundColor = aColor; }

	VCGColors backGroundColor() const { return theBackgroundColor; }

	void textColor (VCGColors aColor) { theTextColor = aColor; }
	VCGColors textColor () const { return theTextColor; }

	void borderColor (VCGColors aColor) { theBorderColor = aColor; }
	VCGColors borderColor() const { return theBorderColor; }

	void shape (NodeShape aShape) { theShape = aShape; }
	NodeShape shape() const { return theShape; }


private:

	void next(VCGNode *node) { theNext = node; }
	VCGNode *next() const { return theNext; }

	const char *theTitle;
	const char *theLabel;
	NodeShape theShape;
	const char *info1, *info2, *info3;
	VCGColors theBackgroundColor, theTextColor, theBorderColor;
	VCGNode *theNext;
};

////////////////////////////////////////////////////////////////////////////
//
// CLASS:        VCGEdge
//
// BASE CLASSES: None.
//
// SUBCLASSES:   None.
//
// FRIENDS:      None.
//
// PURPOSE:	
//     To represent a VCG Edge
//
// NOTES:
//
//////////////////////////////////////////////////////////////////////////

class VCGEdge
{
	friend class VCGGraph;

public:

	VCGEdge(const char *sourceName, const char *targetName);
	~VCGEdge();

	void edgeClass(const UInt32 classId) { theClass = classId; }

	void color (VCGColors aColor) { theColor = aColor; }

	void emit (FILE *) const;

	const char * label() const { return theLabel; }
	void label(const char *val) { theLabel = val; }

	VCGEdgeLineStyle lineStyle() const { return theLineStyle; }
	void lineStyle(VCGEdgeLineStyle val) { theLineStyle = val; }

	void backEdge(Boolean val) { isBackEdge = val; }
	Boolean backEdge() const { return isBackEdge; }

private:

	void next(VCGEdge *aNewEdge) { theNext = aNewEdge; }

	VCGEdge *next() const { return theNext; }

	const char *theSourceName, *theTargetName;
	VCGEdge *theNext;
	UInt32 theClass;
	VCGColors theColor;
	const char *theLabel;
	Boolean isBackEdge:1;
	VCGEdgeLineStyle theLineStyle:3;
}; 

////////////////////////////////////////////////////////////////////////////
//
// CLASS:        VCGGraph
//
// BASE CLASSES: None.
//
// SUBCLASSES:   None.
//
// FRIENDS:      None.
//
// PURPOSE:	
//     To represent a VCG Graph
//
// NOTES:
//
/////////////////////////////////////////////////////////////////////////
class VCGGraph
{
public:

        enum {
            DefaultWidth = 633,
            DefaultHeight = 808,

            // For a subgraph summary node, indicates unspecified
            // width/height.  For a non-subgraph, same as default
            // width/height.
            NoWidth = MAX_UInt_32,
            NoHeight = MAX_UInt_32
        };

	VCGGraph(const char *title,
                 VCGLayoutAlgorithm layout = UnspecifiedLayout,
                 VCGColors color           = LightGrey,
                 UInt32 width              = NoWidth,
                 UInt32 height             = NoHeight);
	~VCGGraph();

	// Append the graph specification to a file. Generates GDL.
	void emit(FILE *) const;

        // Write graph specification to file. Generates GDL.
        // Overwrites existing file.
        void emit(const char *fname) const;

	void addNode(VCGNode &aNode);
	void addEdge(VCGEdge &anEdge);
	void addSubGraph(VCGGraph &aSubGraph);

	// Create a new edge class. The name string is not duplicated and
        // and it is the client's responsibility to ensure that the lifetime 
        // of the string spans the lifetime of the VCG graph.
	UInt32 edgeClass(const char *name, Boolean hidden = FALSE);

	// If the nodes in the graph have any specific information in them,
	// then this is where you specify the name of that information.
	void infoName(UInt32 infoId, const char *name);

	// Returns the name associated with an information.
	const char *infoName(int infoId) const;

	// If a subgraph, initially folded?
	void folding(UInt32 val) { theFolding = val; }

	// Next subgraph.
	void next(VCGGraph *aNewGraph) { theNext = aNewGraph; }
	VCGGraph *next() const { return theNext; }

	// Should layout perform finetuning ?
	Boolean fineTuning() const { return doFineTuning; }
	void fineTuning(const Boolean val) { doFineTuning = val; }

	// Horizontal distance between nodes in pixels.
	UInt32 xspace() const { return theXspace; }
	void xspace(const UInt32 val) { theXspace = val; }

	// Vertical distance between nodes in pixels.
	UInt32 yspace() const { return theYspace; }
	void yspace(const UInt32 val) { theYspace = val; }

	UInt32 width() const
        {
            return ((theWidth == NoWidth) && !isSubgraph ? DefaultWidth : theWidth);
        }
	void width(const UInt32 val) { theWidth = val; }

	UInt32 height() const
        {
            return ((theHeight == NoHeight) && !isSubgraph ? DefaultHeight : theHeight);
        }
	void height(const UInt32 val) { theHeight = val; }

	// Background color of the graph pallette.
	VCGColors color() const { return theColor; }
	void color(const VCGColors val) { theColor = val; }

	// The layout algorithm to use.
	VCGLayoutAlgorithm layout() const { return theLayout; }
	void layout(const VCGLayoutAlgorithm val) { theLayout = val; }

	const char* title() const { return theTitle; }
	void title(const char *val) { theTitle = val; }

	// Should near edges be allowed ?
	Boolean allowNearEdges() const { return nearEdges; }
	void allowNearEdges(const Boolean val) { nearEdges = val; }

	// Are there edge labels to be displayed ?
	Boolean displayEdgeLabels() const { return theDisplayEdgeLabels; }
	void displayEdgeLabels(const Boolean val)
	    { theDisplayEdgeLabels = val; }

	// Should these labels be laid out as part of the layout, or can
	// they be snuck in after the layout has been done ?
	Boolean displayEdgeLabelsLate() const { return theEdgeLabelsLate; }
	void displayEdgeLabelsLate(const Boolean val)
	    { theEdgeLabelsLate = val; }

	// Use a single point (arrow head) for all incoming edges.
	Boolean portSharing() const { return thePortSharing; }
	void portSharing(const Boolean val) { thePortSharing = val; }

	// Use splines to draw edges instead of polygons.
	Boolean splines() const { return theSplines; }
	void splines(const Boolean val) { theSplines = val; }

	// Force straight long edgs with gradient 90 deg during node placement
	Boolean priorityPhase() const { return thePriorityPhase; }
	void priorityPhase(const Boolean val) { thePriorityPhase = val; }

	// Properties when this subgraph is collapsed to a summary
	// node.  Ignored for non-subgraph.  At present, xvcg does not
	// support a border color for a subgraph -- it's implicitly
	// the same as the text color.
	void textColor (VCGColors aColor) { theTextColor = aColor; }
	VCGColors textColor () const { return theTextColor; }
	//
	void shape (NodeShape aShape) { theShape = aShape; }
	NodeShape shape() const { return theShape; }
	//
	void info(UInt32 infoId, const char *information);

	// Dump the VCG to a file and optionally display it
	void dump(const char *file, Boolean displayNow = FALSE) const;

        int nodeCount() { return theNodeCount; } 
        int edgeCount() { return theEdgeCount; } 

private:
	// The names of the various edges.
	struct className {
		className(const char *name, Boolean hidden = FALSE)
			: theName(name), isHidden(hidden), theNext(NULL) {}

		~className()
		{ 
                   if (theNext != NULL) 
                      delete theNext; 
                }

		const char *theName;
		Boolean isHidden;
		struct className *theNext;
	} *theClassNames;

	const char *theTitle;
	VCGNode *theNodes;
	VCGEdge *theEdges;
	VCGGraph *theSubGraphs;
	VCGGraph *theNext;
	UInt32 theNumberOfEdgeClasses;

	VCGLayoutAlgorithm theLayout;
	VCGColors theColor, theTextColor;
	NodeShape theShape;
	const char *info1, *info2, *info3;

	// The global information tags for the nodes.
	const char *theInfo1Name;
	const char *theInfo2Name;
	const char *theInfo3Name;

	UInt32 theWidth, theHeight;
	UInt32 theXspace, theYspace;

	UInt32  doFineTuning:1;
	UInt32  nearEdges:1;
	UInt32  theDisplayEdgeLabels:1;
	UInt32  theEdgeLabelsLate:1;
	UInt32  thePortSharing:1;
	UInt32  thePriorityPhase:1;
	UInt32  theSplines:1;
	UInt32  theFolding:1;
	Boolean isSubgraph:1;
        int theNodeCount; 
        int theEdgeCount; 

};



#endif /* __VCG_H */
