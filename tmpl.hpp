#pragma once

#include <type_traits>

namespace TMPL
{

///////////////////////////////////////////////////////////////////////////////
// Contains
///////////////////////////////////////////////////////////////////////////////

template<typename T, typename ...Ts>
struct IsOneOf;

template<typename T, template<class...> class U, class... Us>
struct IsOneOf<T, U<Us...>> : std::bool_constant<std::disjunction_v<std::is_same<T, Us>...>> {  };

template<typename T, typename U>
constexpr static inline  bool IsOneOf_v { IsOneOf<T, U>::value };

template<class T, class U>
struct IsSubsetOf;

template<template<class...> class T, template<class...> class U, class... Ts>
struct IsSubsetOf<T<>, U<Ts...>> : std::true_type {  };

template<template<class...> class T, class... Types, class U>
struct IsSubsetOf<T<Types...>, U>
    : std::bool_constant<std::conjunction_v<IsOneOf<Types, U>...>> {  };

template<class T, class U>
constexpr static inline bool IsSubsetOf_v { IsSubsetOf<T, U>::value };

///////////////////////////////////////////////////////////////////////////////
// Type at
///////////////////////////////////////////////////////////////////////////////

template<std::size_t I, class... Ts>
struct TypeAt;

template<std::size_t I, class T, class... Ts>
struct TypeAt<I, T, Ts...> : TypeAt<I - 1, Ts...> {  };

template<class T, class... Ts>
struct TypeAt<0, T, Ts...> { using type = T; };

template<std::size_t I, class... Ts>
using TypeAt_t = typename TypeAt<I, Ts...>::type;

///////////////////////////////////////////////////////////////////////////////
// Index of
///////////////////////////////////////////////////////////////////////////////

template<std::size_t I, class T, class... Ts>
struct IndexOfIMPL;

template<std::size_t I, class T, class U, class... Us>
struct IndexOfIMPL<I, T, U, Us...>
    : std::conditional_t<std::is_same_v<T, U>
                       , std::integral_constant<std::size_t, I>
                       , IndexOfIMPL<I + 1, T, Us...>> {  };

template<class T, class... Ts>
struct IndexOf : IndexOfIMPL<0, T, Ts...> {  };

template<class T, class... Ts>
constexpr static inline auto IndexOf_v { IndexOf<T, Ts...>::value };

///////////////////////////////////////////////////////////////////////////////
// AreUnique
///////////////////////////////////////////////////////////////////////////////

template<class... Ts>
struct AreUnique;

template<class T0, class T1, class ...Tn>
struct AreUnique<T0, T1, Tn...>
    : std::bool_constant<not std::disjunction_v<std::is_same<T0, T1>,
                                                std::is_same<T0, Tn>...,
                                                std::is_same<T1, Tn>...,
                                                std::bool_constant<!AreUnique<Tn...>::value>>>
                                               {  };

template<class T>
struct AreUnique<T> : public std::true_type {  };

template<>
struct AreUnique<>  : public std::true_type {  };

template<class... Ts>
constexpr static inline auto AreUnique_v { AreUnique<Ts...>::value };

} // namespace TMPL
