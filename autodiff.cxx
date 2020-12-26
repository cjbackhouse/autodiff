//g++ -std=c++1z autodiff.cxx

#include <iostream>
#include <utility>

// Default - may be specialized by user
template<int X> std::string var_name()
{
  return std::to_string(X);
}

template<int X> class OneDiff
{
public:
  OneDiff() {}

  OneDiff(double d) : diff(d) {}

  void Print(std::ostream& os) const
  {
    os << "d/d" << var_name<X>() << " = " << diff << std::endl;
  }

  double diff;
};

template<int... Xs> class Diffs;

// Constant value, no derivs
template<> class Diffs<>
{
public:
  Diffs() {}

  Diffs(double v) : val(v) {}

  template<int... Xs> Diffs<Xs...> operator+(const Diffs<Xs...>& d) const
  {
    //    std::cout << __PRETTY_FUNCTION__ << std::endl;
    Diffs<Xs...> ret = d;
    ret.val += val;
    return ret;
  }

  void Print(std::ostream& os) const {os << "Value = " << val << std::endl;}
  double val;
};

template<int i, int... Xs> double get_diff(const Diffs<Xs...>& d)
{
  return ((OneDiff<i>*)&d)->diff;
}

template<int i, int... Xs> double set_diff(const Diffs<Xs...>& d, double val)
{
  ((OneDiff<i>*)&d)->diff = val;
}

template<int... Ys> void CopyDiffs(const Diffs<>& from, Diffs<Ys...>& to)
{
}

template<int X, int... Xs, int... Ys> void CopyDiffs(const Diffs<X, Xs...>& from, Diffs<Ys...>& to)
{
  set_diff<X>(to, get_diff<X>(from));
  CopyDiffs(Diffs<Xs...>(from), to);
}

// Add d/dX = d to b (X < Xs[0]) TODO figure out static_assert
template<int X, int... Xs> Diffs<X, Xs...> Concat(double d,
                                                  const Diffs<Xs...>& b)
{
  //  std::cout << __PRETTY_FUNCTION__ << std::endl;

  Diffs<X, Xs...> ret;
  ret.val = b.val;
  CopyDiffs(b, ret);
  set_diff<X>(ret, d);
  return ret;
}

template<int X, int... Xs> class Diffs<X, Xs...> : protected OneDiff<X>, protected Diffs<Xs...>
{
public:
  // Friend with all other variants of self
  template<int... Ys> friend class Diffs;
  template<int Y, int... Ys> friend Diffs<Y, Ys...> Concat(double, const Diffs<Ys...>&);
  template<int Y, int... Ys, int... Zs> friend void CopyDiffs(const Diffs<Y, Ys...>&, Diffs<Zs...>& to);

  Diffs() {}

  template<class... Args> Diffs(double v, double d, Args... ds) : OneDiff<X>(d), Diffs<Xs...>(v, ds...) {}

  void Print(std::ostream& os) const
  {
    OneDiff<X>::Print(os);
    Diffs<Xs...>::Print(os);
  }

  template<int Y, int... Ys> auto operator+(const Diffs<Y, Ys...>& d) const
  {
    //    std::cout << __PRETTY_FUNCTION__ << std::endl;

    if constexpr(X < Y){
      return Concat<X>(get_diff<X>(*this),
                       Diffs<Xs...>(*this) + d);
    }
    else if constexpr(X == Y){
      return Concat<X>(get_diff<X>(*this) + get_diff<X>(d),
                       Diffs<Xs...>(*this) + Diffs<Ys...>(d));
    }
    else{ // X > Y
      return Concat<Y>(get_diff<Y>(d),
                       *this + Diffs<Ys...>(d));
    }
  }
};

template<int... Xs> std::ostream& operator<<(std::ostream& os, const Diffs<Xs...>& d)
{
  d.Print(os);
  return os;
}

// Specialize
template<> std::string var_name<1>(){return "x";}
template<> std::string var_name<2>(){return "y";}
template<> std::string var_name<3>(){return "z";}

int main()
{
  Diffs<1> d(7., 2.);

  Diffs<1, 2, 3> e(8., 3., 4., 5.);

  Diffs<2, 3> f(1., 6., 7.);

  std::cout << "d:\n" << d << std::endl;
  std::cout << "e:\n" << e << std::endl;
  std::cout << "f:\n" << f << std::endl;


  std::cout << "d+e:\n" << (d+e) << std::endl;

  std::cout << "d+f:\n" << (d+f) << std::endl;

  std::cout << "e+f:\n" << (e+f) << std::endl;

  //  std::cout << "d*e:\n" << (d*e) << std::endl;

  return 0;
}
