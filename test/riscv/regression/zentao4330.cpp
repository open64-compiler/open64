class XalanNode {
public:
  enum NodeType {};
};
class XPath {
  enum eMatchScore {};
  class NodeTester {
    NodeTester();
    typedef eMatchScore (NodeTester::*TestFunctionPtr)(
        const XalanNode &, XalanNode::NodeType) const;
    eMatchScore testDefault(const XalanNode &, XalanNode::NodeType) const;
    TestFunctionPtr m_testFunction;
  };
};
XPath::NodeTester::NodeTester() : m_testFunction(&NodeTester::testDefault) {}
