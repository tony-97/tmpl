#pragma once

#include "tmpl.hpp"
#include "type_list.hpp"

#include <type_traits>
#include <utility>

namespace TMPL
{

namespace Sequence
{

///////////////////////////////////////////////////////////////////////////////
// Type at index
///////////////////////////////////////////////////////////////////////////////

template<std::size_t I, class Seq_t>
struct TypeAt;

template<std::size_t I, template<class...> class Seq_t, class... Ts>
struct TypeAt<I, Seq_t<Ts...>> : TMPL::TypeAt<I, Ts...> {  };

template<std::size_t I, class Seq_t>
using TypeAt_t = typename TypeAt<I, Seq_t>::type;

///////////////////////////////////////////////////////////////////////////////
// Index Of
///////////////////////////////////////////////////////////////////////////////

template<class T, class Seq_t>
struct IndexOf;

template<class T, template<class...> class Seq_t, class... Ts>
struct IndexOf<T, Seq_t<Ts...>> : TMPL::IndexOf<T, Ts...> {  };

template<class T, class Seq_t>
constexpr static inline auto IndexOf_v { IndexOf<T, Seq_t>::value };

///////////////////////////////////////////////////////////////////////////////
// Size
///////////////////////////////////////////////////////////////////////////////

template<class Seq_t>
struct Size;

template<template <class...> class Seq_t, class... Types_t>
struct Size<Seq_t<Types_t...>>
    : std::integral_constant<std::size_t, sizeof...(Types_t)> {  };

template<class Seq_t>
constexpr static inline auto Size_v = Size<Seq_t>::value;

///////////////////////////////////////////////////////////////////////////////
// For each type on the type list
///////////////////////////////////////////////////////////////////////////////

template<class Seq_t>
struct ForEach_t;

template<template<class...> class Seq_t>
struct ForEach_t<Seq_t<>>
{
    template<class Callable_t, class... Args_t>
    constexpr static auto
    Do([[maybe_unused]]Callable_t&& callable, [[maybe_unused]]Args_t&&... args)
    -> void {  }
};

template<template<class...>class Seq_t, class Head_t, class... Tail_t>
struct ForEach_t<Seq_t<Head_t, Tail_t...>>
{
    template<class Callable_t, class... Args_t>
    constexpr static auto
    Do(Callable_t&& callable, Args_t&&... args)
    -> void
    {
        callable.template operator()<Head_t>(std::forward<Args_t>(args)...);
        ForEach_t<Seq_t<Tail_t...>>::Do(std::forward<Callable_t>(callable),
                                          std::forward<Args_t>(args)...);
    }
};

///////////////////////////////////////////////////////////////////////////////
// Concatenate Type Lists
///////////////////////////////////////////////////////////////////////////////

template<class Seq1_t, class Seq2_t>
struct SeqCatIMPL;

template<template <class...> class Seq1_t, class... Ts,
         template <class...> class Seq2_t, class... Us>
struct SeqCatIMPL<Seq1_t<Ts...>, Seq2_t<Us...>>
{
    using type = TMPL::TypeList_t<Ts..., Us...>;
};

template<class Seq1_t, class Seq2_t>
using SeqCatIMPL_t = typename SeqCatIMPL<Seq1_t, Seq2_t>::type;

template<class... Seqs_t>
struct SeqCat;

template<template<class...> class Seq_t, class... Types>
struct SeqCat<Seq_t<Types...>>
{
    using type = Seq_t<Types...>;
};

template<class Seq1_t, class Seq2_t>
struct SeqCat<Seq1_t, Seq2_t> : SeqCatIMPL<Seq1_t, Seq2_t> {  };

template<class First_t, class Second_t, class... Rest_t>
struct SeqCat<First_t, Second_t, Rest_t...>
    : SeqCat<SeqCatIMPL_t<First_t, Second_t>, Rest_t...> {  };

template<class... Seqs_t>
using SeqCat_t = typename SeqCat<Seqs_t...>::type;

///////////////////////////////////////////////////////////////////////////////
// Pass types as template argument
///////////////////////////////////////////////////////////////////////////////

template<class...> struct Unpacker_t;

template<template<class...> class Seq_t, class... Args_t>
struct Unpacker_t<Seq_t<Args_t...>>
{
    template<class Functor_t, class... FArgs_t>
    using FunctorReturn_t = decltype(
            std::declval<Functor_t>().template
                operator()<Args_t...>(std::declval<FArgs_t>()...));

    template<class Functor_t, class... FArgs_t>
    constexpr static auto
    Call(Functor_t&& callable, FArgs_t&&... args)
    -> FunctorReturn_t<Functor_t, FArgs_t...>
    {
        return callable.template
            operator()<Args_t...>(std::forward<FArgs_t>(args)...);
    }
};

///////////////////////////////////////////////////////////////////////////////
// Type list with unique types
///////////////////////////////////////////////////////////////////////////////

template<typename T> struct TypeIdentity { using type = T; };
template<typename T> using TypeIdentity_t = typename TypeIdentity<T>::type;

template <typename SeqOut_t, typename... Ts>
struct UniqueTypesIMPL : TypeIdentity<SeqOut_t> { };

template <template<class...> class SeqOut_t,
          typename... Ts,
          typename U,
          typename... Us>
struct UniqueTypesIMPL<SeqOut_t<Ts...>, U, Us...>
    : std::conditional_t<(std::is_same_v<U, Ts> || ...)
                         , UniqueTypesIMPL<SeqOut_t<Ts...>, Us...>
                         , UniqueTypesIMPL<SeqOut_t<Ts..., U>, Us...>> {  };

template <class Seq_t>
struct UniqueTypes;

template <template<class...>class Seq_t, typename... Ts>
struct UniqueTypes<Seq_t<Ts...>> : public UniqueTypesIMPL<Seq_t<>, Ts...> { };

template <class Seq_t>
using UniqueTypes_t = typename UniqueTypes<Seq_t>::type;

///////////////////////////////////////////////////////////////////////////////
// Substract a element of the sequence 
///////////////////////////////////////////////////////////////////////////////

template<class SeqOut_t, class... Ts>
struct RemoveTypesIMPL
{
    using type = SeqOut_t;
};

template<template<class...> class SeqOut_t, class... Ts,
         template<class...> class SeqA_t, class A, class... As,
         template<class...> class SeqB_t, class... Bs>
struct RemoveTypesIMPL<SeqOut_t<Ts...>, SeqA_t<A, As...>, SeqB_t<Bs...>>
    : std::conditional_t<(std::is_same_v<A, Bs> || ...),
                         RemoveTypesIMPL<SeqOut_t<Ts...>, SeqA_t<As...>, SeqB_t<Bs...>>,
                         RemoveTypesIMPL<SeqOut_t<Ts..., A>, SeqA_t<As...>, SeqB_t<Bs...>>> {  };

template<class Seq1_t, class Seq2_t>
struct RemoveTypes
    : RemoveTypesIMPL<TMPL::TypeList_t<>, Seq1_t, Seq2_t> {  };

template<class Seq1_t, class Seq2_t>
using RemoveTypes_t = typename RemoveTypes<Seq1_t, Seq2_t>::type;

///////////////////////////////////////////////////////////////////////////////
// Ignore order compare type list 
///////////////////////////////////////////////////////////////////////////////

template <typename T, typename Tuple>
struct TypeCounter;

template <typename T, template<class...> class Seq_t, typename ... Ts>
struct TypeCounter<T, Seq_t<Ts...>>
    : std::integral_constant<std::size_t, (std::is_same_v<T, Ts> + ...)> {  };

template <typename T, typename Seq_t>
constexpr static inline std::size_t TypeCounter_v { TypeCounter<T, Seq_t>::value };

template <typename FirstSeq_t, typename SecondSeq_t, std::size_t... Is>
constexpr bool IEqualTypes(std::index_sequence<Is...>)
{
    return (...
            && (   TypeCounter_v<TypeAt<Is, FirstSeq_t>, FirstSeq_t>
                == TypeCounter_v<TypeAt<Is, FirstSeq_t>, SecondSeq_t>)
           );
}

template <typename FirstSeq_t, typename SecondSeq_t>
constexpr bool IEqualTypes()
{
    constexpr auto s1 = Size_v<FirstSeq_t>;
    constexpr auto s2 = Size_v<SecondSeq_t>;

    return s1 == s2
           && IEqualTypes<FirstSeq_t,
                          SecondSeq_t>(std::make_index_sequence<s1>());
}

} // namespace Sequence

} // namespace TMPL
