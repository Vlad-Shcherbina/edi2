use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use winapi::shared::minwindef::{LPARAM, LRESULT, UINT, WPARAM};
use winapi::shared::windef::HWND;

/// A reminder that window proc is reentrant.
///
/// Common ways to observe reentrant calls include:
///
/// * Calling `DestroyWindow`.
///
/// * Calling `SendMessage`.
///
/// * Calling a synchronous dialog, including a file dialog.
///
/// Safe wrappers for such functions should take an argument
/// of type `Reent`.
pub struct Reent<'a>(std::marker::PhantomData<&'a mut ()>);

pub struct StateRef<'s, S>(&'s RefCell<S>);

/// Allows to get either mutable state, or reentrancy token,
/// but not at the same time.
impl<'s, S> StateRef<'s, S> {
    pub fn state_mut<'a>(&'a mut self)
    -> impl Deref<Target=S> + DerefMut<Target=S> + 'a {
        self.0.borrow_mut()
    }

    pub fn reent<'a>(&'a mut self) -> Reent<'a> {
        Reent(std::marker::PhantomData)
    }
}

pub trait WindowProcState: Sized {
    fn window_proc(
        sr: StateRef<Self>,
        hwnd: HWND, msg: UINT, wparam: WPARAM, lparam: LPARAM,
    )-> Option<LRESULT>;
}

pub struct OuterState<S>(RefCell<S>);

impl<S> OuterState<S> {
    pub fn new(state: S) -> Self {
        Self(RefCell::new(state))
    }
}

impl<S: WindowProcState> win_win::WindowProc for OuterState<S> {
    fn window_proc(&self, hwnd: HWND, msg: UINT, wparam: WPARAM, lparam: LPARAM,
    ) -> Option<LRESULT> {
        S::window_proc(StateRef(&self.0), hwnd, msg, wparam, lparam)
    }
}
