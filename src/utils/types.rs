#![allow(dead_code)] 

pub enum Either<A, B> {
    Left(A),
    Right(B)
} 

impl<A, B> Either<A, B> {
    pub fn is_left(&self) -> bool {
        match self {
            Either::Left(_) => true,
            Either::Right(_) => false
        }
    }

    pub fn is_right(&self) -> bool {
        !self.is_left()
    }

    pub fn unwrap_left(&self) -> &A {
        match self {
            Either::Left(x) => x,
            Either::Right(_) => panic!("Unwrapped left, when Either was right")
        }
    }

    pub fn unwrap_right(&self) -> &B {
        match self {
            Either::Right(x) => x,
            Either::Left(_) => panic!("Unwrapped right, when Either was left")
        }
    }
}
