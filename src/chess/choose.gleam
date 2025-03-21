/// `case` handling with results, but in function form for less nesting.
/// Similar conceptually to `bool.lazy_guard`, but rather than selecting
/// one of two anonymous functions which take no inputs, this takes a
/// result, and two functions that each accept the values within that
/// result. It then calls the proper function based on whether that result
/// was Ok() or an Error(), passing the value contained in that result
/// to the function.
///
/// This function is specifically designed to replace a `case` expression
/// on a result in your code and decrease the nesting, thanks to the `use`
/// keyword. Given this case statement:
///
/// ```gleam
/// case result {
///   Error(val) -> {
///     // handle the error condition
///   }
///   Ok(val) -> {
///    // handle the ok condition
///   }
/// }
/// ```
///
/// This function can change this code into:
///
/// ```gleam
/// let error_condition = fn(err) {
///   // handle error
/// }
/// use unwrapped_value <- cases(result, on_error: error_condition)
/// // the rest of the function is now dedicated to handling the `ok`
/// // condition
/// ```
///
/// This lets you keep a linear control flow and handle an Ok or Error
/// result early, and keep the rest of the code dedicated to the "main"
/// case.
pub fn cases(
  result: Result(val, err),
  on_ok ok_condition: fn(val) -> shared_return_type,
  on_error error_condition: fn(err) -> shared_return_type,
) -> shared_return_type {
  case result {
    Ok(value) -> ok_condition(value)
    Error(err) -> error_condition(err)
  }
}
