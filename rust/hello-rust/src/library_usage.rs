extern crate rary;

// 11.1, 11.2
// The `-L .` argument adds the current directory to the library search path
// $ rustc -L . library_usage.rs && ./library_usage
fn main() {
    rary::public_function();
    rary::indirect_access();
}
