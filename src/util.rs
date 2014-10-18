pub trait BoolMap {
	fn map<T>(self, f: || -> T) -> Option<T>;
}

impl BoolMap for bool {
	fn map<T>(self, f: || -> T) -> Option<T> {
		if self {
			Some(f())
		} else {
			None
		}
	}
}
