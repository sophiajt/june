struct Node {
    data: i64,
    next: Option<*mut Node>,
}

struct CircularList {
    node: Option<*mut Node>,
}

impl Drop for CircularList {
    fn drop(&mut self) {
        use std::alloc::{dealloc, Layout};
        use std::ptr;

        match self.node {
            Some(ptr) => {
                let start = ptr;
                let mut current = ptr;

                loop {
                    let next = unsafe { (*current).next.expect("internal error") };

                    unsafe {
                        ptr::drop_in_place(current);
                        dealloc(current as *mut u8, Layout::new::<Node>());
                    }

                    if next == start {
                        break;
                    } else {
                        current = next;
                    }
                }
            }
            None => {}
        }
    }
}

impl CircularList {
    pub fn default() -> Self {
        Self { node: None }
    }

    pub fn push(&mut self, data: i64) {
        match self.node {
            Some(ptr) => unsafe {
                let next = (*ptr).next;
                (*ptr).next = Some(Box::into_raw(Box::new(Node { data, next })));
            },
            None => {
                self.node = Some(Box::into_raw(Box::new(Node { data, next: None })));
                unsafe { (*self.node.expect("internal error")).next = self.node };
            }
        }
    }

    pub fn next(&mut self) -> i64 {
        match self.node {
            Some(ptr) => {
                let ret = unsafe { (*ptr).data };

                self.node = Some(unsafe { (*ptr).next.expect("internal error") });

                ret
            }
            None => 0,
        }
    }
}

fn helper() -> CircularList {
    let list = CircularList::default();

    list
}

fn main() {
    let mut list = helper();

    list.push(1);
    list.push(2);
    list.push(3);

    println!("{}", list.next());
    println!("{}", list.next());
    println!("{}", list.next());
    println!("{}", list.next());
    println!("{}", list.next());
    println!("{}", list.next());
}
