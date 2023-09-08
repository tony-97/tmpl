#pragma once

#include <type_traits>

namespace TMPL {

template<typename T, class... Us> struct IsOneOf : std::disjunction<std::is_same<T, Us>...>
{};

template<typename T, class... Us> constexpr static inline bool IsOneOf_v{ IsOneOf<T, Us...>::value };

///////////////////////////////////////////////////////////////////////////////
// Type at
///////////////////////////////////////////////////////////////////////////////

template<std::size_t I, class... Ts> struct TypeAt;

template<std::size_t I, class T, class... Ts> struct TypeAt<I, T, Ts...> : TypeAt<I - 1, Ts...>
{};

template<class T, class... Ts> struct TypeAt<0, T, Ts...>
{
  using type = T;
};

template<std::size_t I, class... Ts> using TypeAt_t = typename TypeAt<I, Ts...>::type;

///////////////////////////////////////////////////////////////////////////////
// Index of
///////////////////////////////////////////////////////////////////////////////

template<std::size_t I, class T, class... Ts> struct IndexOfIMPL;

template<std::size_t I, class T, class U, class... Us>
struct IndexOfIMPL<I, T, U, Us...>
  : std::conditional_t<std::is_same_v<T, U>, std::integral_constant<std::size_t, I>, IndexOfIMPL<I + 1, T, Us...>>
{};

template<class T, class... Ts> struct IndexOf : IndexOfIMPL<0, T, Ts...>
{};

template<class T, class... Ts> constexpr static inline auto IndexOf_v{ IndexOf<T, Ts...>::value };

///////////////////////////////////////////////////////////////////////////////
// AreUnique
///////////////////////////////////////////////////////////////////////////////

template<class... Ts> struct AreUnique;

template<class T0, class T1, class... Tn>
struct AreUnique<T0, T1, Tn...>
  : std::bool_constant<not std::disjunction_v<std::is_same<T0, T1>,
                                              std::is_same<T0, Tn>...,
                                              std::is_same<T1, Tn>...,
                                              std::bool_constant<!AreUnique<Tn...>::value>>>
{};

template<class T> struct AreUnique<T> : public std::true_type
{};

template<> struct AreUnique<> : public std::true_type
{};

template<class... Ts> constexpr static inline auto AreUnique_v{ AreUnique<Ts...>::value };

} // namespace TMPL
