//g++ -std=c++1z autodiff.cxx

#include <iostream>
#include <utility>

// Default - may be specialized by user
template<int X> std::string var_name()
{
  return std::to_string(X);
}

template<int X> struct OneDiff
{
  double diff;
};


template<int... Xs> class Diffs : protected OneDiff<Xs>...
{
public:
  void Print(std::ostream& os) const
  {
    os << "Value = " << val << std::endl;
    Print<Xs...>(os);
  }

  template<int i> const double& diff() const{return OneDiff<i>::diff;}
  template<int i>       double& diff()      {return OneDiff<i>::diff;}

  double val;

 protected:
  template<int Y, int... Ys> void Print(std::ostream& os) const
  {
    os << "  d/d" << var_name<Y>() << " = " << diff<Y>() << std::endl;
    if constexpr(sizeof...(Ys) > 0) Print<Ys...>(os);
  }
};

template<int... Xs> struct Indices;

template<> struct Indices<>
{
  static const bool empty = true;
  static const int head = std::numeric_limits<int>::max(); // TODO bad hack
  static const Indices<> tail;

  typedef Diffs<> Diffs_t;
};

template<int X, int... Xs> struct Indices<X, Xs...>
{
  static const int head = X;
  static const Indices<Xs...> tail;
  static const bool empty = false;

  typedef Diffs<X, Xs...> Diffs_t;
};


template<class A, class B> struct ConcatT;
template<class A, class B> using Concat_t = typename ConcatT<A, B>::type;

template<int... Xs, int... Ys> struct ConcatT<Indices<Xs...>, Indices<Ys...>>
{
  typedef Indices<Xs..., Ys...> type;
};

template<class A, class B> struct ZipT;
template<class A, class B> using Zip_t = typename ZipT<A, B>::type;

template<> struct ZipT<Indices<>, Indices<>>
{
  typedef Indices<> type;
};

template<int... Xs> struct ZipT<Indices<Xs...>, Indices<>>
{
  typedef Indices<Xs...> type;
};

template<int... Ys> struct ZipT<Indices<>, Indices<Ys...>>
{
  typedef Indices<Ys...> type;
};

template<int XY, int... Xs, int... Ys> struct ZipT<Indices<XY, Xs...>, Indices<XY, Ys...>>
{
  typedef Concat_t<Indices<XY>, Zip_t<Indices<Xs...>, Indices<Ys...>>> type;
};

template<int X, int... Xs, int Y, int... Ys> struct ZipT<Indices<X, Xs...>, Indices<Y, Ys...>>
{
  typedef std::conditional_t<(X < Y),
    Concat_t<Indices<X>, Zip_t<Indices<Xs...>, Indices<Y, Ys...>>>,
    Concat_t<Indices<Y>, Zip_t<Indices<X, Xs...>, Indices<Ys...>>>> type;
};

template<int... Xs, int... Ys> void CopyDiffs(Indices<> is, const Diffs<Xs...>& from, Diffs<Ys...>& to)
{
}

template<int... Zs, int... Xs, int... Ys> void CopyDiffs(Indices<Zs...> is, const Diffs<Xs...>& from, Diffs<Ys...>& to)
{
  to.template diff<is.head>() = from.template diff<is.head>();
  /*if constexpr(sizeof...(Zs) > 0)*/ CopyDiffs(is.tail, from, to);
}

// Add d/dX = d to b (X < Xs[0]) TODO figure out static_assert
template<int X, int... Xs> Diffs<X, Xs...> Concat(double d,
                                                  const Diffs<Xs...>& b)
{
  Diffs<X, Xs...> ret;
  ret.val = b.val;
  CopyDiffs(Indices<Xs...>(), b, ret);
  ret.template diff<X>() = d;
  return ret;
}

template<class Op, int... Xs, int... Ys, int... Ws, int... Zs>
typename Zip_t<Indices<Xs...>, Indices<Ys...>>::Diffs_t
ZipWith(Indices<Xs...> i, Indices<Ys...> j,
        const Diffs<Ws...>& a, const Diffs<Zs...>& b)
{
  if constexpr(i.empty && j.empty){
    Diffs<> ret;
    ret.val = Op::op(a.val, 0, b.val, 0).val;
    return ret;
  }
  else{
    if constexpr(j.empty || i.head < j.head){
      return Concat<i.head>(Op::op(a.val, a.template diff<i.head>(),
                                   b.val, 0).diff,
                            ZipWith<Op>(i.tail, j, a, b));
    }

    if constexpr(i.head == j.head){
      return Concat<i.head>(Op::op(a.val, a.template diff<i.head>(),
                                   b.val, b.template diff<i.head>()).diff,
                            ZipWith<Op>(i.tail, j.tail, a, b));
    }

    if constexpr(i.empty || j.head < i.head){
      return Concat<j.head>(Op::op(a.val, 0,
                                   b.val, b.template diff<j.head>()).diff,
                            ZipWith<Op>(i, j.tail, a, b));
    }
  }

  abort();
}

template<class Op, int... Xs, int... Ys>
typename Zip_t<Indices<Xs...>, Indices<Ys...>>::Diffs_t
ZipWith(const Diffs<Xs...>& a, const Diffs<Ys...>& b)
{
  return ZipWith<Op>(Indices<Xs...>(), Indices<Ys...>(), a, b);
}

struct OpRet
{
  double val; double diff;
};

struct AddOp
{
  static OpRet op(double u, double du, double v, double dv)
  {
    return {u+v, du+dv};
  }
};

struct SubOp
{
  static OpRet op(double u, double du, double v, double dv)
  {
    return {u-v, du-dv};
  }
};

struct MulOp
{
  static OpRet op(double u, double du, double v, double dv)
  {
    return {u*v, u*dv + v*du};
  }
};

struct DivOp
{
  static OpRet op(double u, double du, double v, double dv)
  {
    return {u/v, (v*du - u*dv ) / (v*v)};
  }
};


template<int... Xs, int... Ys> auto
operator+(const Diffs<Xs...>& a, const Diffs<Ys...>& b)
{
  return ZipWith<AddOp>(a, b);
}

template<int... Xs, int... Ys> auto
operator-(const Diffs<Xs...>& a, const Diffs<Ys...>& b)
{
  return ZipWith<SubOp>(a, b);
}

template<int... Xs, int... Ys> auto
operator*(const Diffs<Xs...>& a, const Diffs<Ys...>& b)
{
  return ZipWith<MulOp>(a, b);
}

template<int... Xs, int... Ys> auto
operator/(const Diffs<Xs...>& a, const Diffs<Ys...>& b)
{
  return ZipWith<DivOp>(a, b);
}

template<int... Xs> std::ostream& operator<<(std::ostream& os, const Diffs<Xs...>& d)
{
  d.Print(os);
  return os;
}

// Specialize names
template<> std::string var_name<1>(){return "x";}
template<> std::string var_name<2>(){return "y";}
template<> std::string var_name<3>(){return "z";}

int main()
{
  Diffs<1> d;
  d.val = 7;
  d.diff<1>() = 2;

  Diffs<1, 2, 3> e;
  e.val = 8;
  e.diff<1>() = 3;
  e.diff<2>() = 4;
  e.diff<3>() = 5;

  Diffs<2, 3> f;
  f.val = 1;
  f.diff<2>() = 6;
  f.diff<3>() = 7;

  std::cout << "d:\n" << d << std::endl;

  std::cout << "e:\n" << e << std::endl;
  std::cout << "f:\n" << f << std::endl;


  std::cout << "d+e:\n" << (d+e) << std::endl;

  std::cout << "d+f:\n" << (d+f) << std::endl;

  std::cout << "e+f:\n" << (e+f) << std::endl;


  std::cout << "d*e:\n" << (d*e) << std::endl;

  std::cout << "d*f:\n" << (d*f) << std::endl;

  std::cout << "e*f:\n" << (e*f) << std::endl;


  return 0;
}
