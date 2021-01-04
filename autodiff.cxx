//g++ -std=c++1z autodiff.cxx

#include <array>
#include <iostream>
#include <limits>
#include <tuple>

// Default - may be specialized by user
template<int X> std::string var_name()
{
  return std::to_string(X);
}


class Abstract
{
private:
  Abstract() {}
};

template<int... Xs> struct Indices;
template<int... Xs> class Diffs;

template<> struct Indices<>: public Abstract
{
  static const bool empty = true;
  static const int head = std::numeric_limits<int>::max(); // TODO bad hack

  static const int size = 0;

  typedef Diffs<> Diffs_t;

  // Don't implement IndexOf() here. Accessing an invalid index will then be a
  // failure to find Indices<>::IndexOf().
};

template<int X, int... Xs> struct Indices<X, Xs...>: public Abstract
{
  static const bool empty = false;
  static const int head = X;
  typedef Indices<Xs...> tail_t;

  static const int size = sizeof...(Xs)+1;

  typedef Diffs<X, Xs...> Diffs_t;

  template<int Y> static constexpr/*consteval*/ int IndexOf()
  {
    if constexpr(Y == X)
      return 0;
    else
      return 1+tail_t::template IndexOf<Y>();}
};


template<int... Xs> class Diffs
{
  using Idxs = Indices<Xs...>;

public:
  Diffs() {} // default uninitialized(!)

  void Print(std::ostream& os) const
  {
    os << "Value = " << val << std::endl;
    Print<Xs...>(os);
  }

  template<int i> const double& diff() const{return diffs[Idxs::template IndexOf<i>()];}
  template<int i>       double& diff()      {return diffs[Idxs::template IndexOf<i>()];}

  double val;

protected:
  template<int Y, int... Ys> void Print(std::ostream& os) const
  {
    os << "  d/d" << var_name<Y>() << " = " << diff<Y>() << std::endl;
    if constexpr(sizeof...(Ys) > 0) Print<Ys...>(os);
  }

  std::array<double, Idxs::size> diffs;

  // Grant Concat() access to this constructor
  template<int Y, int... Ys> friend Diffs<Y, Ys...> Concat(double d, const Diffs<Ys...>& b);

  Diffs(double v, const decltype(diffs)& d) : val(v), diffs(d) {}
};


template<class A, class B> struct ConcatT;
template<class A, class B> using Concat_t = typename ConcatT<A, B>::type;

template<int... Xs, int... Ys> struct ConcatT<Indices<Xs...>, Indices<Ys...>>: public Abstract
{
  typedef Indices<Xs..., Ys...> type;
};

template<class A, class B> struct ZipT;
template<class A, class B> using Zip_t = typename ZipT<A, B>::type;

template<> struct ZipT<Indices<>, Indices<>>: public Abstract
{
  typedef Indices<> type;
};

template<int... Xs> struct ZipT<Indices<Xs...>, Indices<>>: public Abstract
{
  typedef Indices<Xs...> type;
};

template<int... Ys> struct ZipT<Indices<>, Indices<Ys...>>: public Abstract
{
  typedef Indices<Ys...> type;
};

template<int XY, int... Xs, int... Ys> struct ZipT<Indices<XY, Xs...>, Indices<XY, Ys...>>: public Abstract
{
  typedef Concat_t<Indices<XY>, Zip_t<Indices<Xs...>, Indices<Ys...>>> type;
};

template<int X, int... Xs, int Y, int... Ys> struct ZipT<Indices<X, Xs...>, Indices<Y, Ys...>>: public Abstract
{
  using IX = Indices<X, Xs...>;
  using IY = Indices<Y, Ys...>;

  typedef std::conditional_t<(X < Y),
    Concat_t<Indices<X>, Zip_t<Indices<Xs...>, IY>>,
    Concat_t<Indices<Y>, Zip_t<IX, Indices<Ys...>>>> type;
};

// Add d/dX = d to b (X < Xs[0])
template<int X, int... Xs> Diffs<X, Xs...> Concat(double d,
                                                  const Diffs<Xs...>& b)
{
  static_assert(X < Indices<Xs...>::head);

  return Diffs<X, Xs...>(b.val,
                         std::apply([d](auto... n){
                           return std::array<double, sizeof...(n)+1>{d, n...};},
                           b.diffs));
}

template<class Op, class IDXA, class IDXB> struct ZipWithS;

template<class Op, int... Xs, int... Ys> struct
ZipWithS<Op, Indices<Xs...>, Indices<Ys...>>: public Abstract
{
  typedef typename Zip_t<Indices<Xs...>, Indices<Ys...>>::Diffs_t Res_t;

  template<int... Ws, int... Zs> static Res_t Zip(const Diffs<Ws...>& a, const Diffs<Zs...>& b)
  {
    using i = Indices<Xs...>;
    using j = Indices<Ys...>;

    if constexpr(i::empty && j::empty){
      Diffs<> ret;
      ret.val = Op::op(a.val, 0, b.val, 0).val;
      return ret;
    }
    else{
      if constexpr(j::empty || (i::head < j::head)){
        return Concat<i::head>(Op::op(a.val, a.template diff<i::head>(),
                                      b.val, 0).diff,
                               ZipWithS<Op, typename i::tail_t, j>::Zip(a, b));
      }

      if constexpr(i::head == j::head){
        return Concat<i::head>(Op::op(a.val, a.template diff<i::head>(),
                                      b.val, b.template diff<i::head>()).diff,
                               ZipWithS<Op, typename i::tail_t, typename j::tail_t>::Zip(a, b));
      }

      if constexpr(i::empty || j::head < i::head){
        return Concat<j::head>(Op::op(a.val, 0,
                                      b.val, b.template diff<j::head>()).diff,
                               ZipWithS<Op, i, typename j::tail_t>::Zip(a, b));
      }
    }

    abort();
  }
};

template<class Op, int... Xs, int... Ys>
typename Zip_t<Indices<Xs...>, Indices<Ys...>>::Diffs_t
ZipWith(const Diffs<Xs...>& a, const Diffs<Ys...>& b)
{
  return ZipWithS<Op, Indices<Xs...>, Indices<Ys...>>::Zip(a, b);
}

struct OpRet
{
  double val; double diff;
};

struct AddOp: public Abstract
{
  static OpRet op(double u, double du, double v, double dv)
  {
    return {u+v, du+dv};
  }
};

struct SubOp: public Abstract
{
  static OpRet op(double u, double du, double v, double dv)
  {
    return {u-v, du-dv};
  }
};

struct MulOp: public Abstract
{
  static OpRet op(double u, double du, double v, double dv)
  {
    return {u*v, u*dv + v*du};
  }
};

struct DivOp: public Abstract
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
