pub trait BoolMap {
	fn map<T, F>(self, f: F) -> Option<T>
        where F: FnOnce() -> T;
}

impl BoolMap for bool {
	fn map<T, F>(self, f: F) -> Option<T>
        where F: FnOnce() -> T
    {
		if self {
			Some(f())
		} else {
			None
		}
	}
}
