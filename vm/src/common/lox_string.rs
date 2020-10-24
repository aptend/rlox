use std::rc::Rc;

// LoxString is immutable. It should be cheap to clone into stack.
// Rc<String> has 8 bytes, which keeps the size of Value 16 bytes
// Compare:
//   String has 24 bytes: ptr + cap + len
//   Rc<str> has 16 bytes, it's a fat pointer, ptr + len
#[derive(Debug, Clone)]
pub struct LoxString(Rc<String>);

impl LoxString {
    // !! these two converters must be called after global intern.
    // Could have impl them using std::converter::From, but it is better
    // to make them more explicit and less visible
    pub(crate) fn from_owned(s: String) -> Self {
        LoxString(Rc::new(s))
    }
    // TODO: AsRef
    pub(crate) fn from_ref(s: &str) -> Self {
        LoxString(Rc::new(s.to_string()))
    }
}

impl std::cmp::PartialEq for LoxString {
    // intern strings makes it correct to do ptr comparing
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl std::hash::Hash for LoxString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl std::cmp::Eq for LoxString {}

// Hash matches between LoxString and str, but Eq doesn't
// This is Ok because we have interned **all** LoxStrings
// There is no way to have 2 different LoxStrings with the same literal str.
impl std::borrow::Borrow<str> for LoxString {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl std::ops::Deref for LoxString {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
