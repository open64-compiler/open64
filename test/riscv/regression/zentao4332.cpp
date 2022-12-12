namespace std {
template <typename _Tp> _Tp *__addressof(_Tp &);
template <typename = int> class vector;
struct _Bvector_base {
  struct {
    int _M_end_of_storage;
    void _M_end_addr() { __addressof(_M_end_of_storage); }
  } _M_impl;
};
template <> class vector<> : _Bvector_base {
public:
  void push_back() { _M_impl._M_end_addr(); }
};
} // namespace std
class FormatterToXMLBase {
protected:
  std::vector<> m_elemStack;
};
class FormatterToXML_UTF16 : FormatterToXMLBase {
  void startElement();
};
void FormatterToXML_UTF16::startElement() { m_elemStack.push_back(); }
