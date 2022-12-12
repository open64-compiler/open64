namespace std {
template <typename _Tp> _Tp forward(_Tp);
int piecewise_construct;
template <typename...> class tuple;
template <unsigned long...> struct _Index_tuple;
template <typename _T1, typename> struct pair {
  _T1 first;
  template <typename... _Args1, typename... _Args2>
  pair(int, tuple<_Args1...>, tuple<_Args2...>);
  template <typename... _Args1, unsigned long... _Indexes1, typename... _Args2,
            unsigned long... _Indexes2>
  pair(tuple<_Args1...> &, tuple<_Args2...> &, _Index_tuple<_Indexes1...>,
       _Index_tuple<_Indexes2...>);
};
} // namespace std
struct new_allocator {
  template <typename _Up, typename... _Args>
  void construct(_Up *, _Args... __args) {
    _Up(__args...);
  }
};
namespace std {
struct allocator_traits {
  template <typename _Up, typename... _Args>
  static void construct(_Up __p, _Args... __args) {
    new_allocator __a;
    __a.construct(__p, __args...);
  }
};
pair<const int *, int> *_M_valptr();
struct _Rb_tree_const_iterator {
  _Rb_tree_const_iterator(int);
};
template <typename, typename, typename, typename, typename = int>
struct _Rb_tree {
  template <typename... _Args> void _M_construct_node(int, _Args... __args) {
    pair<const int *, int> __trans_tmp_1 = *_M_valptr();
    allocator_traits::construct(&__trans_tmp_1, __args...);
  }
  template <typename... _Args> void _M_create_node(_Args... __args) {
    _M_construct_node(0, __args...);
  }
  typedef int iterator;
  template <typename... _Args>
  iterator _M_emplace_hint_unique(_Rb_tree_const_iterator, _Args &&...);
};
template <typename _Key, typename _Val, typename _KeyOfValue, typename _Compare,
          typename _Alloc>
template <typename... _Args>
int _Rb_tree<_Key, _Val, _KeyOfValue, _Compare, _Alloc>::_M_emplace_hint_unique(
    _Rb_tree_const_iterator, _Args &&...__args) {
  _M_create_node(__args...);
}
template <typename> struct tuple_element;
template <long, typename _Tp>
using __tuple_element_t = typename tuple_element<_Tp>::type;
template <unsigned long...> struct _Index_tuple {};
template <int _Num> struct _Build_index_tuple {
  template <typename, long...> using _IdxTuple = _Index_tuple<1>;
  using __type = __make_integer_seq<_IdxTuple, long, _Num>;
};
template <typename...> struct tuple {};
template <typename _Head, typename... _Tail>
struct tuple_element<tuple<_Head, _Tail...>> {
  typedef _Head type;
};
template <long __i, typename... _Elements>
__tuple_element_t<__i, tuple<_Elements...>> get(tuple<_Elements...>);
template <class _T1, class _T2>
template <typename... _Args1, typename... _Args2>
pair<_T1, _T2>::pair(int, tuple<_Args1...> __first, tuple<_Args2...> __second)
    : pair(__first, __second,
           typename _Build_index_tuple<sizeof...(_Args1)>::__type(),
           typename _Build_index_tuple<sizeof...(_Args2)>::__type()) {}
template <class _T1, class _T2>
template <typename... _Args1, unsigned long... _Indexes1, typename... _Args2,
          unsigned long... _Indexes2>
pair<_T1, _T2>::pair(tuple<_Args1...> &__tuple1, tuple<_Args2...> &,
                     _Index_tuple<_Indexes1...>, _Index_tuple<_Indexes2...>)
    : first(forward<_Args1>(get<_Indexes1>(__tuple1))...) {}
template <typename _Key> struct map {
  typedef _Key key_type;
  typedef _Rb_tree<key_type, pair<_Key, int>, int, _Key> _Rep_type;
  _Rep_type _M_t;
  void operator[](key_type) {
    tuple<int *&> __trans_tmp_3;
    typename _Rep_type::iterator __i = _M_t._M_emplace_hint_unique(
        __i, piecewise_construct, __trans_tmp_3, tuple<>());
  }
};
} // namespace std
int startNode___trans_tmp_2;
std::map<int *> m_map;
void NamespaceNodesTreeWalkerstartNode() { m_map[&startNode___trans_tmp_2]; }
