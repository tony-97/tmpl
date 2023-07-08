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
// Pack the types to antoher list
///////////////////////////////////////////////////////////////////////////////
template<template<class...> class T, class Seq_t>
struct ConvertTo;

template<template<class...> class T, template<class...> class Seq_t, class... Ss>
struct ConvertTo<T, Seq_t<Ss...>>
{
    using type = T<Ss...>;
};

template<template<class...> class T, class Seq_t>
using ConvertTo_t = typename ConvertTo<T, Seq_t>::type;

///////////////////////////////////////////////////////////////////////////////
// Is Unique types
///////////////////////////////////////////////////////////////////////////////

template<class Seq_t>
struct IsUnique;

template<template<class... >class Seq_t, class... Ts>
struct IsUnique<Seq_t<Ts...>> : TMPL::AreUnique<Ts...> {  };

template<class Seq_t>
constexpr static inline auto IsUnique_v { IsUnique<Seq_t>::value };

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
// Check if types are unique
///////////////////////////////////////////////////////////////////////////////

template<class Seq_t>
struct AreUnique;

template<template<class...> class Seq_t, class... Ts>
struct AreUnique<Seq_t<Ts...>> : TMPL::AreUnique<Ts...> {  };

template<class Seq_t>
constexpr static inline auto AreUnique_v { AreUnique<Seq_t>::value };

///////////////////////////////////////////////////////////////////////////////
// For each type on the type list
///////////////////////////////////////////////////////////////////////////////

template<class Seq_t>
struct ForEach_t;

template<template<class...>class Seq_t, class... Ts>
struct ForEach_t<Seq_t<Ts...>>
{
    template<class Callable_t, class... Args_t>
    constexpr static auto
    Do(Callable_t&& callable, Args_t&&... args)
    -> void
    {
        (callable.template operator()<Ts>(std::forward<Args_t>(args)...), ...);
    }
};

///////////////////////////////////////////////////////////////////////////////
// Map the types of the Type Lists
///////////////////////////////////////////////////////////////////////////////

template<class Seq_t, template<class...> class Apply_t>
struct Map;

template<template<class...>class Seq_t, class... Ts, template<class...> class Apply_t>
struct Map<Seq_t<Ts...>, Apply_t>
{
    using type = Seq_t<typename Apply_t<Ts>::type...>;
};

template<class Seq_t, template<class...> class Apply_t>
using Map_t = typename Map<Seq_t, Apply_t>::type;

///////////////////////////////////////////////////////////////////////////////
// Concatenate Type Lists
///////////////////////////////////////////////////////////////////////////////

template<class Seq1_t, class Seq2_t>
struct CatIMPL;

template<template <class...> class Seq1_t, class... Ts,
         template <class...> class Seq2_t, class... Us>
struct CatIMPL<Seq1_t<Ts...>, Seq2_t<Us...>>
{
    using type = TMPL::TypeList_t<Ts..., Us...>;
};

template<class Seq1_t, class Seq2_t>
using CatIMPL_t = typename CatIMPL<Seq1_t, Seq2_t>::type;

template<class... Seqs_t>
struct Cat;

template<template<class...> class Seq_t, class... Types>
struct Cat<Seq_t<Types...>>
{
    using type = Seq_t<Types...>;
};

template<class Seq1_t, class Seq2_t>
struct Cat<Seq1_t, Seq2_t> : CatIMPL<Seq1_t, Seq2_t> {  };

template<class First_t, class Second_t, class... Rest_t>
struct Cat<First_t, Second_t, Rest_t...>
    : Cat<CatIMPL_t<First_t, Second_t>, Rest_t...> {  };

template<class... Seqs_t>
using Cat_t = typename Cat<Seqs_t...>::type;

///////////////////////////////////////////////////////////////////////////////
// Pass types as template argument
///////////////////////////////////////////////////////////////////////////////

template<class...> struct Unpacker_t;

template<template<class...> class Seq_t, class... TArgs_t>
struct Unpacker_t<Seq_t<TArgs_t...>>
{
    template<class Functor_t, class... FArgs_t>
    using FunctorReturn_t = decltype(
            std::declval<Functor_t>().template
                operator()<TArgs_t...>(std::declval<FArgs_t>()...));

    template<class Functor_t, class... FArgs_t>
    constexpr static auto
    Call(Functor_t&& callable, FArgs_t&&... args)
    -> FunctorReturn_t<Functor_t, FArgs_t...>
    {
        return callable.template
            operator()<TArgs_t...>(std::forward<FArgs_t>(args)...);
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
