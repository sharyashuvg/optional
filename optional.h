#pragma once
#include<type_traits>
#include<optional>
#include<concepts>
#include<cstdlib>
#include<utility>

namespace shar {
	
	namespace detail {
		struct _nontrivial_dummy_type {
			constexpr _nontrivial_dummy_type() noexcept {
				// This default constructor is user-provided to avoid zero-initialization when objects are value-initialized.
			}
		};
		struct empty {};

		struct ignore_t { // struct that ignores assignments
			template <class T>
			constexpr const ignore_t& operator=(const T&) const noexcept /* strengthened */ {
				// do nothing
				return *this;
			}
		};

		template<typename T, typename... Args>
		decltype(auto) construct_at(T& obj, Args&&... args) {
			return std::construct_at(std::addressof(obj), std::forward<Args>(args)...);
		}

		template <class T, template <class...> class TemplatedType>
		struct is_specialization : std::false_type {};

		template <class... Args, template <class...> class TemplatedType>
		struct is_specialization<TemplatedType<Args...>, TemplatedType> : std::true_type {};

		template <class T, template <class...> class Template>
		inline constexpr bool is_specialization_v = is_specialization<T, Template>::value;

		namespace derived_from_specialization_detail {
			template <class... T>
			static constexpr bool always_false = false;

			template <template <class...> class TemplatedType, class... ARGS>
			void _derived_from_specialization_impl(const TemplatedType<ARGS...>&) {
				static_assert(always_false<ARGS>,"cannot be used in evaluated context"); return {};
			}

		}

		template <class obj, template <class...> class TemplatedType>
		static constexpr bool derived_from_specialization_of = requires(obj b) { { derived_from_specialization_detail::_derived_from_specialization_impl(b) }; };


		namespace swap_no_adl {
			void swap() = delete;

			template <class T>
			inline constexpr bool is_trivially_swappable_v = std::is_trivially_destructible_v<T> &&
				std::is_trivially_move_constructible_v<T> && std::is_trivially_move_assignable_v<T> &&
				!requires (T& a) { swap(a, a); };
		}
		using swap_no_adl::is_trivially_swappable_v;

	}

	template <typename T>
	/*[[trivial_abi]]*/ class optional 
	{
	public:

		using value_type = T;
		using iterator = T*;
		using const_iterator = const T*;

		constexpr optional(std::nullopt_t = std::nullopt) noexcept : _contains(false), _dummy{} {}

		constexpr optional(const optional& other) requires std::is_trivially_copy_constructible_v<T> = default;
		constexpr optional(const optional& other) noexcept(std::is_nothrow_copy_constructible_v<T>)  
													requires std::is_copy_constructible_v<T> && !std::is_trivially_copy_constructible_v<T> 
													: _contains(other.has_value())
		{
			if (_contains)
			{
				this->construct_value( other._value);
			}
		}

		constexpr optional(optional&& other) requires std::is_trivially_move_constructible_v<T> = default;
		
		constexpr optional(optional&& other) noexcept(std::is_nothrow_move_constructible_v<T>)
												requires std::is_move_constructible_v<T> && !std::is_trivially_move_constructible_v<T>
												: _contains(other.has_value())
		{
			if (_contains)
			{
				this->construct_value(std::move(other._value));
			}
		}

		/* see https ://en.cppreference.com/w/cpp/utility/optional/optional ctor 4
		 if T is not (possibly cv - qualified) bool, T is not constructible or convertible from any expression of type(possibly const) std::optional<U>, i.e., the following 8 values are all false:
			std::is_constructible_v<T, std::optional<U>&>
			std::is_constructible_v<T, const std::optional<U>&>
			std::is_constructible_v<T, std::optional<U>&&>
			std::is_constructible_v<T, const std::optional<U>&&>
			std::is_convertible_v<std::optional<U>&, T>
			std::is_convertible_v<const std::optional<U>&, T>
			std::is_convertible_v<std::optional<U>&&, T>
			std::is_convertible_v<const std::optional<U>&&, T>
		*/
		template <class U>
		struct allowOptionalUCtor
			: std::bool_constant< std::disjunction_v< std::is_same<std::remove_cv_t<T>, bool>,//if bool bail out
			std::negation< std::disjunction< //(and of nots) is (not of or of everything)
			std::is_same<T, U>, //disable for copy 
			std::is_constructible<T, optional<U>&>,
			std::is_constructible<T, const optional<U>&>,
			std::is_constructible<T, const optional<U>&&>,
			std::is_constructible<T, optional<U>&&>,
			std::is_convertible<optional<U>&, T>,
			std::is_convertible<const optional<U>&, T>,
			std::is_convertible<const optional<U>&&, T>,
			std::is_convertible<optional<U>&&, T> >>>> {};

		template < class U >
		requires std::is_constructible_v<T, const U&> && allowOptionalUCtor<U>::value
		constexpr explicit(!std::is_convertible_v<const U&, T>) 
			optional(const optional<U>& other) noexcept(std::is_nothrow_constructible_v<T, const U&>)
			: _contains(other.has_value())
		{
			if (_contains)
			{
				this->construct_value(*other);
				return;
			}
		}

		template < class U >
			requires std::is_constructible_v<T, U&&> && allowOptionalUCtor<U>::value
		constexpr explicit(!std::is_convertible_v<U&&, T>)
			optional(optional<U>&& other) noexcept(std::is_nothrow_constructible_v<T, U&&>)
			: _contains(other.has_value())
		{
			if (_contains)
			{
				this->construct_value(std::move(*other));
				return;
			}

		}

		template< class... Args >
		constexpr explicit optional(std::in_place_t, Args&&... args)
																	noexcept(std::is_nothrow_constructible_v<T, Args...>) 
																	requires std::is_constructible_v<T, Args...>
																	: _value(_STD forward<Args>(args)...), _contains{ true } {}



		template < class U = T >
		constexpr explicit(!std::is_convertible_v<U&&, T>) optional(U&& value) noexcept(std::is_nothrow_constructible_v<T, U&&>)
																			requires std::is_constructible_v<T, U&&> && 
																						!std::is_same_v<std::remove_cvref_t<U>, std::in_place_t> &&
																						!std::is_same_v<std::remove_cvref_t<U>, optional> && 
																						!(std::is_same_v< std::remove_cv_t<T>, bool> && detail::is_specialization_v<U, optional>)
																							
																							: optional(std::in_place, std::forward<U>(value)) {}
		
		
		constexpr optional& operator=(std::nullopt_t) noexcept{
			if (has_value())
			{
				reset();
			}
			return *this;
		}

		constexpr optional& operator=(const optional& other) requires std::is_trivially_copy_assignable_v<T> && 
																		std::is_trivially_copy_constructible_v<T> && 
																		std::is_trivially_destructible_v<T> = default;

		constexpr optional& operator=(const optional& other) noexcept(std::is_nothrow_copy_assignable_v<T>) {

			*this = other.has_value() ? other._value : std::nullopt;
			return *this;
		}

		constexpr optional& operator=(optional&& other) requires std::is_trivially_move_assignable_v<T> &&
																std::is_trivially_move_constructible_v<T> &&
																std::is_trivially_destructible_v<T> = default;
		constexpr optional& operator=(optional&& other) noexcept(std::is_nothrow_move_assignable_v<T>) {
			
			*this = other.has_value() ? std::move(other._value) : std::nullopt;
			return *this;
		}

		template <class U>
		struct _allow_assignment
			: std::bool_constant<!std::disjunction_v<std::is_same<T, U>, std::is_assignable<T&, optional<U>&>,
			std::is_assignable<T&, const optional<U>&>, std::is_assignable<T&, const optional<U>>,
			std::is_assignable<T&, optional<U>>>> {};

		template <class U>
			requires std::conjunction_v<_allow_assignment<U>,
			std::is_constructible<T, const U&>, std::is_assignable<T&, const U&>>
		constexpr optional& operator=(const optional<U>& other) noexcept(std::is_nothrow_assignable_v<T&, const U&> && std::is_nothrow_constructible_v<T, const U&>) 
		{
				*this = other.has_value() ? other._value : std::nullopt;

			return *this;
		}

		template <class U>
			requires std::conjunction_v<_allow_assignment<U>,
			std::is_constructible<T, U&&>, std::is_assignable<T&, U&&>>
			constexpr optional& operator=(optional<U>&& other) noexcept(std::is_nothrow_assignable_v<T&, U&&> && std::is_nothrow_constructible_v<T, U&&>)
		{
			*this = other.has_value() ? std::move(other._value) : std::nullopt;
			return *this;
		}


		template <class U = T> 
			requires std::conjunction_v<
			std::negation<std::is_same<optional, std::remove_cvref_t<U>>>,
			std::negation<std::conjunction<std::is_scalar<T>, std::is_same<T, std::decay_t<U>>>>,
			std::is_constructible<T, U>, 
			std::is_assignable<T&, U>>

		constexpr optional& operator=(U&& _Right) noexcept( std::is_nothrow_assignable_v<T&, U>&& std::is_nothrow_constructible_v<T, U>)
		{
			if (has_value())
			{
				this->_value = std::forward<U>(_Right);
			}
			else
			{
				this->construct_value(std::forward<U>(_Right));
			}
			return *this;
		}
			
		constexpr ~optional() noexcept requires std::is_trivially_destructible_v<T> = default;

		constexpr ~optional() noexcept(noexcept(this->reset())) requires !std::is_trivially_destructible_v<T> {
			reset();
		}

	private:
		union 
		{
			detail::_nontrivial_dummy_type _dummy;
			std::remove_cvref_t<T> _value;
		};
		bool _contains;
		
		template <class... Args>
		constexpr void construct_value(Args&&... args) noexcept(std::is_nothrow_constructible_v<T, Args...>) 
		{
			detail::construct_at(this->_value, std::forward<Args>(args)...);
			this->_contains = true;
		}

		struct _inplace_invoke_construct_t {};

		template <class F, class... Args>
		constexpr optional(_inplace_invoke_construct_t, F&& f, Args&&... args) noexcept(std::is_nothrow_invocable_v<F, Args...> && std::is_nothrow_constructible_v<optional, std::invoke_result_t<F,Args...>>) 
			: _value(std::invoke(std::forward<F>(f), std::forward<Args>(args)...)), _contains(true)
		{}

	public:

		constexpr void swap(optional& other) noexcept(std::is_nothrow_move_constructible_v<T> && std::is_nothrow_swappable_v<T>)
		{
			static_assert(std::is_move_constructible_v<T>,
				"optional<T>::swap requires T to be move constructible (N4950 [optional.swap]/1).");
			static_assert(!std::is_move_constructible_v<T> || std::is_swappable_v<T>,
				"optional<T>::swap requires T to be swappable (N4950 [optional.swap]/2).");
			
			using std::swap; // to allow adl of T
			if constexpr (false && detail::is_trivially_swappable_v<T>) {
				static_assert (std::is_trivially_destructible_v<optional>&&
					std::is_trivially_move_constructible_v<optional> && std::is_trivially_move_assignable_v<optional>);
				std::swap(*this, other);
			}
			else {
				const bool has = this->has_value();
				if (has == other.has_value()) {
					if (has) {
						swap(**this, *other); // intentional ADL
					}
				}
				else {
					optional& _Source = has ? *this : other;
					optional& _Target = has ? other : *this;
					_Target.construct_value(std::move(*_Source));
					_Source.reset();
				}
			}
		}

		//constexpr void swap2(optional& other) noexcept(std::is_nothrow_move_constructible_v<T>&& std::is_nothrow_swappable_v<T>)
		//{
		//	static_assert(std::is_move_constructible_v<T>,
		//		"optional<T>::swap requires T to be move constructible (N4950 [optional.swap]/1).");
		//	static_assert(!std::is_move_constructible_v<T> || std::is_swappable_v<T>,
		//		"optional<T>::swap requires T to be swappable (N4950 [optional.swap]/2).");
		//	using std::swap; // to allow adl of T
		//	if constexpr (false && detail::is_trivially_swappable_v<T>) {
		//		static_assert (std::is_trivially_destructible_v<optional> &&
		//			std::is_trivially_move_constructible_v<optional> && std::is_trivially_move_assignable_v<optional>);
		//		std::swap(*this, other);
		//	}
		//	else {
		//		int a = this->has_value() << 1 | +other.has_value();
		//		switch (a)
		//		{
		//		case 0b00:
		//			break;
		//		case 0b01:
		//			this->construct_value(std::move(*other));
		//			other.reset();
		//			break;
		//		case 0b10:
		//			other.construct_value(std::move(**this));
		//			this->reset();
		//			break;
		//		case 0b11:
		//			swap(**this, *other); // intentional ADL
		//			break;
		//		default:
		//			std::unreachable();
		//			break;
		//		}
		//	}
		//}

		template <class... ARGS>
		constexpr T& emplace(ARGS&&... args) noexcept(std::is_nothrow_constructible_v<T, ARGS...>) 
		{
			reset();
			return this->construct_value(std::forward<ARGS>(args)...);
		}

		constexpr iterator begin() noexcept 
		{
			return std::addressof(_value);
		}
		constexpr const_iterator begin() const noexcept 
		{
			return std::addressof(_value);
		}
		constexpr iterator end() noexcept 
		{
			return std::addressof(_value) + has_value();
		}
		constexpr const_iterator end() const noexcept 
		{
			return std::addressof(_value) + has_value();
		}

		constexpr T* operator->() noexcept {
			return std::addressof(_value);
		}
		constexpr const T* operator->() const noexcept 
		{
			return std::addressof(_value);
		}

		template <class Self>
		constexpr auto&& operator*(this Self&& self) noexcept
		{
			return std::forward<Self>(self)._value;
		}

		template <class Self>
		constexpr auto&& value(this Self&& self) {
			if (self.has_value()) {
				return std::forward<Self>(self)._value;
			}
			throw std::bad_optional_access();
		}

		constexpr explicit operator bool() const noexcept 
		{
			return has_value();
		}
	
		constexpr bool has_value() const noexcept {
			return _contains;
		}
	
		constexpr void reset() noexcept(noexcept(this->_value.~T())) 
		{
			if (has_value())
			{
				if constexpr (!std::is_trivially_destructible_v<T>)
				{
					this->_value.~T();
				}
				this->_contains = false;
			}
		}

		
		template <std::convertible_to<T> U>
		constexpr T value_or(U&& default_value) const& noexcept(std::is_nothrow_convertible_v<T const&,T> && std::is_nothrow_convertible_v<U, T>) 
		{
			static_assert(std::is_convertible_v<const T&, std::remove_cv_t<T>>,
				"The const overload of optional<T>::value_or requires const T& to be convertible to remove_cv_t<T> "
				"(N4950 [optional.observe]/15 as modified by LWG-3424).");
			static_assert(std::is_convertible_v<U, T>,
				"optional<T>::value_or(U) requires U to be convertible to T (N4950 [optional.observe]/15).");
			return has_value() ? **this : static_cast<T>(std::forward<U>(default_value));
		}

		template <std::convertible_to<T> U>
		constexpr T value_or(U&& default_value)&& noexcept(std::is_nothrow_convertible_v<T &&, T> && std::is_nothrow_convertible_v<U, T>) 
		{
			static_assert(std::is_convertible_v<T, std::remove_cv_t<T>>,
				"The rvalue overload of optional<T>::value_or requires T to be convertible to remove_cv_t<T> "
				"(N4950 [optional.observe]/17 as modified by LWG-3424).");
			static_assert(std::is_convertible_v<U, T>,
				"optional<T>::value_or(U) requires U to be convertible to T (N4950 [optional.observe]/15).");
			return has_value() ? std::move(**this) : static_cast<T>(std::forward<U>(default_value));
		}

		template <typename F, typename Self>
		constexpr auto and_then(this Self&& self,F&& f) noexcept(std::is_nothrow_invocable_v<F, decltype(*std::forward<Self>(self))>&&
														std::is_nothrow_convertible_v<std::invoke_result_t<F, decltype(*std::forward<Self>(self))>, optional<std::invoke_result_t<F, decltype(*std::forward<Self>(self))>>>)
														-> std::invoke_result_t<F, decltype(*std::forward<Self>(self))>
														
		{
			
			static_assert(std::invocable<F, decltype(*std::forward<Self>(self))>,
				"F must be invocable with T& (N4950 [optional.observe]/18).");
			static_assert(detail::is_specialization_v< std::invoke_result_t < F, decltype(*std::forward<Self>(self)) >, optional>,
				"optional<T>::and_then(F) requires the return type of F to be a specialization of optional "
				"(N4950 [optional.monadic]/2).");
			if (has_value()) {
				return std::invoke(std::forward<F>(f), *std::forward<Self>(self));
			}
			return std::remove_cvref_t < std::invoke_result_t < F, decltype(*std::forward<Self>(self)) >> {};
		}

		template <typename F, typename Self>
		constexpr auto transform(this Self&& self, F&& f) noexcept(noexcept(optional<std::remove_cvref_t< std::invoke_result_t<F, decltype(*std::forward<Self>(self))> >>{_inplace_invoke_construct_t{}, std::forward<F>(f), * std::forward<Self>(self)}))
														  -> optional< std::remove_cvref_t < std::invoke_result_t< F, decltype(*std::forward<Self>(self)) >>>
																
		{
			using U = std::remove_cvref_t< std::invoke_result_t<F, decltype(*std::forward<Self>(self))> >;
			if (has_value()) {
				return optional<U>{_inplace_invoke_construct_t{}, std::invoke(std::forward<F>(f), *std::forward<Self>(self))};
			}
			return std::nullopt;
		}

		template <typename F, typename Self>
		constexpr auto map(this Self&& self, F&& f) noexcept(noexcept(std::forward<Self>(self).transform(std::forward<F>(f))))
		{
			return std::forward<Self>(self).transform(std::forward<F>(f));
		}

		template <typename F>
		constexpr optional or_else( F&& f) const& noexcept(std::is_nothrow_invocable_v<F>&& std::is_nothrow_convertible_v< std::invoke_result_t<F>, optional> && std::is_nothrow_convertible_v<optional const&, optional>)
													
		{

			static_assert(std::invocable<F>, "F must be invocable with no arguments (N4950 [optional.observe]/19).");
			static_assert(detail::is_specialization_v< std::invoke_result_t<F>, optional> //standard says it should be optional
				|| std::is_constructible_v<T, std::invoke_result_t<F>>,"optional::or_else callable parameter requires the result type to be specialization of optional"); //non standard but makes sense
			if (has_value()) {
				return *this;
			}
			return std::invoke(std::forward<F>(f));
		}

		template <typename F>
		constexpr optional or_else(F&& f) && noexcept(std::is_nothrow_invocable_v<F> && std::is_nothrow_convertible_v< std::invoke_result_t<F>, optional>&& std::is_nothrow_convertible_v<optional &&, optional>)
		{
			static_assert(std::invocable<F>, "F must be invocable with no arguments (N4950 [optional.observe]/19).");
			static_assert(detail::is_specialization_v< std::invoke_result_t<F>, optional > //standard says it should be optional
				|| std::is_constructible_v<T, std::invoke_result_t<F>>, "optional::or_else callable parameter requires the result type to be specialization of optional"); //non standard but makes sense
			if (has_value()) {
				return std::move(*this);
			}
			return std::invoke(std::forward<F>(f));
		}



	};

	

	template <class T>
	optional(T) -> optional<T>;

//all operators are the same so we generate them(case: two optionals)
#define _shar_binary_optional_relational_operator(op)          \
\
	template <class T1, class T2>  \
		constexpr bool operator op(const optional<T1>& lhs, const optional<T2>& rhs) noexcept( requires { { *lhs op *rhs } noexcept -> ::std::convertible_to<bool>; })  \
																								requires requires { \
																										{ *lhs op *rhs } -> ::std::convertible_to<bool>; \
																								} \
	{ \
		const bool lefs_has_value  = lhs.has_value(); \
		const bool right_has_value = rhs.has_value(); \
		if (lefs_has_value && right_has_value) {      \
			return *lhs op *rhs;                      \
		}                                             \
		return lefs_has_value op right_has_value;     \
	}

//all operators are the same so we generate them(case: two optionals)
#define _shar_unary_prefix_optional_relational_operator(op)          \
\
	template <class T1, class T2>  \
		constexpr bool operator op(const optional<T1>& lhs, const T2& rhs) noexcept( requires { { *lhs op rhs } noexcept -> ::std::convertible_to<bool>; })  \
																								requires requires { \
																										{ *lhs op rhs } -> ::std::convertible_to<bool>; \
																								} \
	{												  \
		if (lhs) {									  \
			return *lhs op rhs;                       \
		}                                             \
		return false;								  \
	}

#define _shar_unary_infix_optional_relational_operator(op)          \
\
	template <class T1, class T2>  \
		constexpr bool operator op(const T1& rhs, const optional<T2>& lhs ) noexcept( requires { { lhs op *rhs } noexcept -> ::std::convertible_to<bool>; })  \
																								requires requires { \
																										{ lhs op *rhs } -> ::std::convertible_to<bool>; \
																								} \
	{												  \
		if (rhs) {									  \
			return lhs op *rhs;                       \
		}                                             \
		return false;								  \
	}


	//for some reason intelisense is not happy with this without the semicolon
	_shar_binary_optional_relational_operator(== );
	_shar_binary_optional_relational_operator(!= ); 
	_shar_binary_optional_relational_operator(< ); 
	_shar_binary_optional_relational_operator(> ); 
	_shar_binary_optional_relational_operator(<= ); 
	_shar_binary_optional_relational_operator(>= ); 
	_shar_unary_infix_optional_relational_operator(== ); 
	_shar_unary_infix_optional_relational_operator(!= ); 
	_shar_unary_infix_optional_relational_operator(< ); 
	_shar_unary_infix_optional_relational_operator(> ); 
	_shar_unary_infix_optional_relational_operator(<= ); 
	_shar_unary_infix_optional_relational_operator(>= ); 
	_shar_unary_prefix_optional_relational_operator(== ); 
	_shar_unary_prefix_optional_relational_operator(!= ); 
	_shar_unary_prefix_optional_relational_operator(< ); 
	_shar_unary_prefix_optional_relational_operator(> ); 
	_shar_unary_prefix_optional_relational_operator(<= ); 
	_shar_unary_prefix_optional_relational_operator(>= ); 


#undef _shar_binary_optional_relational_operator
#undef _shar_unary_prefix_optional_relational_operator
#undef _shar_unary_infix_optional_relational_operator

	template <class T1, std::three_way_comparable_with<T1> T2>  
	constexpr std::compare_three_way_result_t<T1, T2> operator <=>(const optional<T1>& lhs, const optional<T2>& rhs) noexcept(noexcept(*lhs <=> *rhs))
	{
		const bool lefs_has_value = lhs.has_value();
		const bool right_has_value = rhs.has_value();
		if (lefs_has_value && right_has_value) {
			return *lhs <=> *rhs;
		}
		return lefs_has_value <=> right_has_value;

	}


	template< class T >
	constexpr bool operator==(const optional<T>& opt, std::nullopt_t) noexcept {
		return !opt.has_value();
	}
	template< class T >
	constexpr std::strong_ordering operator<=>(const optional<T>& opt, std::nullopt_t) noexcept {
		return opt.has_value() <=> false;
	}

	template <class T1, std::three_way_comparable_with<T1> U>
	requires (!detail::derived_from_specialization_of<U, optional>)
	constexpr std::compare_three_way_result_t<T1, U> operator <=>(const optional<T1>& lhs, const U& rhs) noexcept(noexcept(*lhs <=> rhs))
	{
		if (lhs) {
			return *lhs <=> rhs;
		}
		return std::strong_ordering::less;
	}

	/*template <class T1, class T2>
	constexpr void swap(optional<T1>& lhs, optional<T2>& rhs) noexcept(noexcept(lhs.swap(rhs))) 
															  requires std::swappable_with<T1&, T2&>&& std::swappable_with<T1&, T2&> 
															  && std::move_constructible<T1>&& std::move_constructible<T2>
	{
		lhs.swap(rhs);
	}*/

	template <class T1>
	constexpr void swap(optional<T1>& lhs, optional<T1>& rhs) noexcept(noexcept(lhs.swap(rhs))) requires std::swappable<T1>&& std::move_constructible<T1>
	{
		lhs.swap(rhs);
	}

}

namespace std {


	template<typename T>
	struct hash<::shar::optional<T>> {
		constexpr size_t operator()(const ::shar::optional<T>& o) const noexcept {
			if (o) return std::hash<std::remove_cvref_t<T>>{}(*o);
			return 0;
		}
	};
}