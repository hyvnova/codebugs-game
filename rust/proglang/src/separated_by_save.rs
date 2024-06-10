
use chumsky::{
    debug::Debugger,
    error::{Error,Located},
    stream::Stream,
    Parser,
};

use core::marker::PhantomData;

type PResult<I, O, E> = (
    Vec<Located<I, E>>,
    Result<(O, Option<Located<I, E>>), Located<I, E>>,
);

// Shorthand for a stream with the given input and error type.
type StreamOf<'a, I, E> = Stream<'a, I, <E as Error<I>>::Span>;


/// See [`Parser::separated_by`].
#[must_use]
pub struct SeparatedBySave<A, B, U> {
    pub(crate) item: A,
    pub(crate) delimiter: B,
    pub(crate) at_least: usize,
    pub(crate) at_most: Option<usize>,
    pub(crate) allow_leading: bool,
    pub(crate) allow_trailing: bool,
    pub(crate) phantom: PhantomData<U>,
}

impl<A, B, U> SeparatedBySave<A, B, U> {
    /// Allow a leading separator to appear before the first item.
    ///
    /// Note that even if no items are parsed, a leading separator *is* permitted.
    ///
    /// # Examples
    ///
    /// ```
    /// # use chumsky::prelude::*;
    /// let r#enum = text::keyword::<_, _, Simple<char>>("enum")
    ///     .padded()
    ///     .ignore_then(text::ident()
    ///         .padded()
    ///         .separated_by(just('|'))
    ///         .allow_leading());
    ///
    /// assert_eq!(r#enum.parse("enum True | False"), Ok(vec!["True".to_string(), "False".to_string()]));
    /// assert_eq!(r#enum.parse("
    ///     enum
    ///     | True
    ///     | False
    /// "), Ok(vec!["True".to_string(), "False".to_string()]));
    /// ```
    pub fn allow_leading(mut self) -> Self {
        self.allow_leading = true;
        self
    }

    /// Allow a trailing separator to appear after the last item.
    ///
    /// Note that if no items are parsed, no leading separator is permitted.
    ///
    /// # Examples
    ///
    /// ```
    /// # use chumsky::prelude::*;
    /// let numbers = text::int::<_, Simple<char>>(10)
    ///     .padded()
    ///     .separated_by(just(','))
    ///     .allow_trailing()
    ///     .delimited_by(just('('), just(')'));
    ///
    /// assert_eq!(numbers.parse("(1, 2)"), Ok(vec!["1".to_string(), "2".to_string()]));
    /// assert_eq!(numbers.parse("(1, 2,)"), Ok(vec!["1".to_string(), "2".to_string()]));
    /// ```
    pub fn allow_trailing(mut self) -> Self {
        self.allow_trailing = true;
        self
    }

    /// Require that the pattern appear at least a minimum number of times.
    ///
    /// ```
    /// # use chumsky::prelude::*;
    /// let numbers = just::<_, _, Simple<char>>('-')
    ///     .separated_by(just('.'))
    ///     .at_least(2);
    ///
    /// assert!(numbers.parse("").is_err());
    /// assert!(numbers.parse("-").is_err());
    /// assert_eq!(numbers.parse("-.-"), Ok(vec!['-', '-']));
    /// ````
    pub fn at_least(mut self, n: usize) -> Self {
        self.at_least = n;
        self
    }

    /// Require that the pattern appear at most a maximum number of times.
    ///
    /// ```
    /// # use chumsky::prelude::*;
    /// let row_4 = text::int::<_, Simple<char>>(10)
    ///     .padded()
    ///     .separated_by(just(','))
    ///     .at_most(4);
    ///
    /// let matrix_4x4 = row_4
    ///     .separated_by(just(','))
    ///     .at_most(4);
    ///
    /// assert_eq!(
    ///     matrix_4x4.parse("0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15"),
    ///     Ok(vec![
    ///         vec!["0".to_string(), "1".to_string(), "2".to_string(), "3".to_string()],
    ///         vec!["4".to_string(), "5".to_string(), "6".to_string(), "7".to_string()],
    ///         vec!["8".to_string(), "9".to_string(), "10".to_string(), "11".to_string()],
    ///         vec!["12".to_string(), "13".to_string(), "14".to_string(), "15".to_string()],
    ///     ]),
    /// );
    /// ````
    pub fn at_most(mut self, n: usize) -> Self {
        self.at_most = Some(n);
        self
    }

    /// Require that the pattern appear exactly the given number of times.
    ///
    /// ```
    /// # use chumsky::prelude::*;
    /// let coordinate_3d = text::int::<_, Simple<char>>(10)
    ///     .padded()
    ///     .separated_by(just(','))
    ///     .exactly(3)
    ///     .then_ignore(end());
    ///
    /// // Not enough elements
    /// assert!(coordinate_3d.parse("4, 3").is_err());
    /// // Too many elements
    /// assert!(coordinate_3d.parse("7, 2, 13, 4").is_err());
    /// // Just the right number of elements
    /// assert_eq!(coordinate_3d.parse("5, 0, 12"), Ok(vec!["5".to_string(), "0".to_string(), "12".to_string()]));
    /// ````
    pub fn exactly(mut self, n: usize) -> Self {
        self.at_least = n;
        self.at_most = Some(n);
        self
    }
}

impl<A: Copy, B: Copy, U> Copy for SeparatedBySave<A, B, U> {}
impl<A: Clone, B: Clone, U> Clone for SeparatedBySave<A, B, U> {
    fn clone(&self) -> Self {
        Self {
            item: self.item.clone(),
            delimiter: self.delimiter.clone(),
            at_least: self.at_least,
            at_most: self.at_most,
            allow_leading: self.allow_leading,
            allow_trailing: self.allow_trailing,
            phantom: PhantomData,
        }
    }
}

impl<I: Clone, O, U, A: Parser<I, O, Error = E>, B: Parser<I, U, Error = E>, E: Error<I>>
    Parser<I, (Vec<O>,Vec<U>)> for SeparatedBySave<A, B, U>
{
    type Error = E;

    #[inline]
    fn parse_inner<D: Debugger>(
        &self,
        debugger: &mut D,
        stream: &mut StreamOf<I, E>,
    ) -> PResult<I, (Vec<O>,Vec<U>), E> {
        if let Some(at_most) = self.at_most {
            assert!(
                self.at_least <= at_most,
                "SeparatedBy cannot parse at least {} and at most {}",
                self.at_least,
                at_most
            );
        }

        enum State<I, E> {
            Terminated(Located<I, E>),
            Continue,
        }

        fn parse_or_not<U, B: Parser<I, U, Error = E>, I: Clone, E: Error<I>, D: Debugger>(
            delimiter: &B,
            stream: &mut StreamOf<I, E>,
            debugger: &mut D,
            delims: &mut Vec<U>,
            alt: Option<Located<I, E>>,
        ) -> Option<Located<I, E>> {
            match stream.try_parse(|stream| {
                #[allow(deprecated)]
                debugger.invoke(&delimiter, stream)
            }) {
                // These two paths are successful path so the furthest errors are merged with the alt.
                (d_errors, Ok((d_out, d_alt))) => {delims.push(d_out); merge_alts(alt, merge_alts(d_alt, d_errors))}
                (d_errors, Err(d_err)) => merge_alts(alt, merge_alts(Some(d_err), d_errors)),
            }
        }

        fn parse<O, A: Parser<I, O, Error = E>, I: Clone, E: Error<I>, D: Debugger>(
            item: &A,
            stream: &mut StreamOf<I, E>,
            debugger: &mut D,
            outputs: &mut Vec<O>,
            errors: &mut Vec<Located<I, E>>,
            alt: Option<Located<I, E>>,
        ) -> (State<I, E>, Option<Located<I, E>>) {
            match stream.try_parse(|stream| {
                #[allow(deprecated)]
                debugger.invoke(item, stream)
            }) {
                (mut i_errors, Ok((i_out, i_alt))) => {
                    outputs.push(i_out);
                    errors.append(&mut i_errors);
                    (State::Continue, merge_alts(alt, i_alt))
                }
                (mut i_errors, Err(i_err)) => {
                    errors.append(&mut i_errors);
                    (State::Terminated(i_err), alt)
                }
            }
        }

        let mut outputs = Vec::new();
        let mut delims = Vec::new();
        let mut errors = Vec::new();
        let mut alt = None;

        if self.allow_leading {
            alt = parse_or_not(&self.delimiter, stream, debugger, &mut delims, alt);
        }

        let (mut state, mut alt) =
            parse(&self.item, stream, debugger, &mut outputs, &mut errors, alt);

        let mut offset = stream.save();
        let error: Option<Located<I, E>>;
        loop {
            if let State::Terminated(err) = state {
                error = Some(err);
                break;
            }
            offset = stream.save();

            if self
                .at_most
                .map_or(false, |at_most| outputs.len() >= at_most)
            {
                error = None;
                break;
            }

            match stream.try_parse(|stream| {
                #[allow(deprecated)]
                debugger.invoke(&self.delimiter, stream)
            }) {
                (mut d_errors, Ok((d_out, d_alt))) => {
                    delims.append(d_out);
                    errors.append(&mut d_errors);
                    alt = merge_alts(alt, d_alt);

                    let (i_state, i_alt) =
                        parse(&self.item, stream, debugger, &mut outputs, &mut errors, alt);
                    state = i_state;
                    alt = i_alt;
                }
                (mut d_errors, Err(d_err)) => {
                    errors.append(&mut d_errors);
                    state = State::Terminated(d_err);
                }
            }
        }
        stream.revert(offset);

        if self.allow_trailing && !outputs.is_empty() {
            alt = parse_or_not(&self.delimiter, stream, debugger, &mut delims, alt);
        }

        if outputs.len() >= self.at_least {
            alt = merge_alts(alt, error);
            (errors, Ok(((outputs,delims), alt)))
        } else if let Some(error) = error {
            // In all paths where `State = State::Terminated`, Some(err) is inserted into alt.
            (errors, Err(error))
        } else {
            (errors, Ok(((outputs,delims), alt)))
        }
    }

    #[inline]
    fn parse_inner_verbose(
        &self,
        d: &mut Verbose,
        s: &mut StreamOf<I, E>,
    ) -> PResult<I, (Vec<O>,Vec<U>), E> {
        #[allow(deprecated)]
        self.parse_inner(d, s)
    }
    #[inline]
    fn parse_inner_silent(&self, d: &mut Silent, s: &mut StreamOf<I, E>) -> PResult<I, (Vec<O>,Vec<U>), E> {
        #[allow(deprecated)]
        self.parse_inner(d, s)
    }
}



fn separated_by_save<I, A, U, B>(item:A, delimiter: B) -> SeparatedBySave<A, B, U>
where
    I: Clone,
    A: Sized + Parser<I, _>,
    B: Parser<I, U, Error = A::Error>,
{
    SeparatedBySave {
        item,
        delimiter,
        at_least: 0,
        at_most: None,
        allow_leading: false,
        allow_trailing: false,
        phantom: PhantomData,
    }
}