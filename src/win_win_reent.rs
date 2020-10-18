use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use winapi::shared::minwindef::{LPARAM, LRESULT, UINT, WPARAM};
use winapi::shared::windef::HWND;
use winapi::um::winuser::WM_CREATE;
use once_cell::unsync::OnceCell;
use std::cell::Cell;

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
    pub fn state_mut(&mut self)
    -> impl Deref<Target=S> + DerefMut<Target=S> + '_ {
        self.0.borrow_mut()
    }

    pub fn reent(&mut self) -> Reent {
        Reent(std::marker::PhantomData)
    }
}

pub trait WindowProcState: Sized {
    fn window_proc(
        sr: StateRef<Self>,
        hwnd: HWND, msg: UINT, wparam: WPARAM, lparam: LPARAM,
    )-> Option<LRESULT>;
}

pub struct LazyState<S, F> {
    state: OnceCell<RefCell<S>>,
    init: std::cell::Cell<Option<F>>,
}

impl<S, F: FnOnce(HWND) -> S> LazyState<S, F> {
    pub fn new(init: F) -> Self {
        LazyState {
            state: OnceCell::new(),
            init: Cell::new(Some(init)),
        }
    }
}

impl<S, F> win_win::WindowProc for LazyState<S, F>
where
    S: WindowProcState,
    F: FnOnce(HWND) -> S,
{
    fn window_proc(&self, hwnd: HWND, msg: UINT, wparam: WPARAM, lparam: LPARAM,
    ) -> Option<LRESULT> {
        let state = self.state.get_or_init(|| {
            assert_eq!(msg, WM_CREATE);
            RefCell::new(self.init.take().unwrap()(hwnd))
        });
        S::window_proc(StateRef(state), hwnd, msg, wparam, lparam)
    }
}
