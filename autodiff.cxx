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

// Derive from this to make an unconstructable class
class Abstract
{
private:
  Abstract() {}
};

// A list of integers, and ability to map an element to its position
template<int... Xs> struct Indices;
// The main class, a value and derivatives wrt to various variables
template<int... Xs> class Diffs;

// Base class of Indices - the empty set
template<> struct Indices<>: public Abstract
{
  static const bool empty = true;
  static const int head = std::numeric_limits<int>::max(); // TODO bad hack
  typedef void head_t;
  typedef void tail_t;

  static const int size = 0;

  // The Diffs object representing this set of variables
  typedef Diffs<> Diffs_t;

  // Don't implement IndexOf() here. Accessing an invalid index will then be a
  // failure to find Indices<>::IndexOf().
};

template<int X, int... Xs> struct Indices<X, Xs...>: public Abstract
{
  static const bool empty = false;
  static const int head = X;
  typedef Indices<X> head_t;
  typedef Indices<Xs...> tail_t;

  // List of indices must be sorted
  static_assert(X < tail_t::head);

  static const int size = sizeof...(Xs)+1;

  typedef Diffs<X, Xs...> Diffs_t;

  template<int Y> static constexpr/*consteval*/ int IndexOf()
  {
    if constexpr(Y == X)
      return 0; // it is the head of the list
    else
      return 1+tail_t::template IndexOf<Y>(); // else recurse
  }
};

template<int... Xs> class Diffs
{
  using Idxs = Indices<Xs...>;

public:
  Diffs() {} // default uninitialized(!)

  static constexpr Diffs Make(double val, const std::array<double, sizeof...(Xs)>& arr)
  {
    return Diffs(val, arr);
  }

  void Print(std::ostream& os) const
  {
    os << "Value = " << val << std::endl;
    Print<Xs...>(os);
  }

  // Translate indexed variables to the element within our diff vector
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

  Diffs(double v, const decltype(diffs)& d) : val(v), diffs(d) {}
};

// trivial concatenation of two sequences of indices
template<class A, class B> struct ConcatT;
template<class A, class B> using Concat_t = typename ConcatT<A, B>::type;

template<int... Xs, int... Ys> struct ConcatT<Indices<Xs...>, Indices<Ys...>>: public Abstract
{
  typedef Indices<Xs..., Ys...> type;
};

// Zipping of two series of indices (equivalent of sort | uniq)
template<class A, class B> struct ZipT;
template<class A, class B> using Zip_t = typename ZipT<A, B>::type;

// Zip({}, {}) = {}
template<> struct ZipT<Indices<>, Indices<>>: public Abstract
{
  typedef Indices<> type;
};

// Zip(x, {}) = x
template<int... Xs> struct ZipT<Indices<Xs...>, Indices<>>: public Abstract
{
  typedef Indices<Xs...> type;
};

// Zip({}, y) = y
template<int... Ys> struct ZipT<Indices<>, Indices<Ys...>>: public Abstract
{
  typedef Indices<Ys...> type;
};

// Sequences share the same prefix
template<int XY, int... Xs, int... Ys> struct ZipT<Indices<XY, Xs...>, Indices<XY, Ys...>>: public Abstract
{
  typedef Concat_t<Indices<XY>, Zip_t<Indices<Xs...>, Indices<Ys...>>> type;
};

// One sequence or the other has the smaller initial index
template<int... Xs, int... Ys> struct ZipT<Indices<Xs...>, Indices<Ys...>>: public Abstract
{
  using IX = Indices<Xs...>;
  using IY = Indices<Ys...>;

  // The first element of the result
  using head_t = std::conditional_t<(IX::head < IY::head),
                                    typename IX::head_t, typename IY::head_t>;

  // Zip together everything that remains, recursively
  using tail_t = std::conditional_t<(IX::head < IY::head),
                                    Zip_t<typename IX::tail_t, IY>,
                                    Zip_t<IX, typename IY::tail_t>>;

  using type = Concat_t<head_t, tail_t>;
};

// Concat(w, [x, y, z]) = [w, x, y, z]
template<size_t N> constexpr std::array<double, N+1> Concat(double d, const std::array<double, N>& arr)
{
  return std::apply([d](auto... n){return std::array<double, sizeof...(n)+1>{d, n...};}, arr);
}

// Struct supporting the ZipWith() function
template<class Op, class IDXA, class IDXB> struct ZipWithS;

template<class Op, int... Xs, int... Ys> struct
ZipWithS<Op, Indices<Xs...>, Indices<Ys...>>: public Abstract
{
  using Z_t = Zip_t<Indices<Xs...>, Indices<Ys...>>;

  // Result of the zipping operation
  using Res_t = std::array<double, Z_t::size>;

  // Returns the result of zipping just indices Ws.. from a and Zs... from
  // b. Zipping here means applying the operator to combine each pair of
  // entries sharing the same index.
  template<int... Ws, int... Zs> static constexpr Res_t Zip(const Diffs<Ws...>& a, const Diffs<Zs...>& b)
  {
    using i = Indices<Xs...>;
    using j = Indices<Ys...>;

    using itail_t = typename i::tail_t;
    using jtail_t = typename j::tail_t;

    // Base case
    if constexpr(i::empty && j::empty){
      return {};
    }
    else{
      // The head of i has no counterpart in j
      if constexpr(j::empty || (i::head < j::head)){
        return Concat(Op::diff_op(a.val, a.template diff<i::head>(), b.val, 0),
                      ZipWithS<Op, itail_t, j>::Zip(a, b));
      }

      // Combine the heads of the two lists and recurse
      if constexpr(i::head == j::head){
        return Concat(Op::diff_op(a.val, a.template diff<i::head>(),
                                  b.val, b.template diff<i::head>()),
                      ZipWithS<Op, itail_t, jtail_t>::Zip(a, b));
      }

      // The head of j has no counterpart in i
      if constexpr(i::empty || j::head < i::head){
        return Concat(Op::diff_op(a.val, 0, b.val, b.template diff<j::head>()),
                      ZipWithS<Op, i, jtail_t>::Zip(a, b));
      }
    }

    __builtin_unreachable;
  }
};

// Wrap all that up in a friendlier function that automatically applies to all
// indices.
template<class Op, int... Xs, int... Ys> constexpr auto
ZipWith(const Diffs<Xs...>& a, const Diffs<Ys...>& b)
{
  using IX = Indices<Xs...>;
  using IY = Indices<Ys...>;

  using Res_t = typename Zip_t<IX, IY>::Diffs_t;

  return Res_t::Make(Op::val_op(a.val, b.val),
                     ZipWithS<Op, IX, IY>::Zip(a, b));
}

// Implement derivatives for the basic arithmetic operators

struct AddOp: public Abstract
{
  static double val_op(double u, double v){return u+v;}
  static double diff_op(double, double du, double, double dv){return du+dv;}
};

struct SubOp: public Abstract
{
  static double val_op(double u, double v){return u-v;}
  static double diff_op(double, double du, double, double dv){return du-dv;}
};

struct MulOp: public Abstract
{
  static double val_op(double u, double v){return u*v;}
  static double diff_op(double u, double du, double v, double dv)
  {
    return u*dv + v*du;
  }
};

struct DivOp: public Abstract
{
  static double val_op(double u, double v){return u/v;}
  static double diff_op(double u, double du, double v, double dv)
  {
    return (v*du - u*dv ) / (v*v);
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
