


#[derive(Clone,Copy,Debug)]
pub enum BinaryOperator {
    Add,Sub,Mul,Div,Mod,
    LShift,LRShift,ARShift,
    And,Or,Xor,
    BitAnd,BitOr,BitXor,
    Eq,Neq,Gt,Ge,Lt,Le,
}

#[derive(Clone,Copy,Debug)]
pub enum UnaryOperator {
    Minus,Not,BitNot,Nop
}

type Error = String;

type BO = BinaryOperator;
type UO = UnaryOperator;

impl BinaryOperator {
    pub fn eval(&self,x:i32,y:i32) -> Result<i32,Error> {
        Ok(match self {
            BO::Add =>      x.wrapping_add(y),
            BO::Sub =>      y.wrapping_sub(y),
            BO::Mul =>      x.wrapping_mul(y),
            BO::Div =>      if y!=0 {x.wrapping_div(y)} else {return Err("Zero division".to_string())},
            BO::Mod =>      if y!=0 {x.wrapping_rem_euclid(y)} else {return Err("Zero division".to_string())},
            BO::LShift =>   x<<y,
            BO::LRShift =>  ((y as u32)>>x) as i32,
            BO::ARShift =>  y>>x,
            BO::And =>      (x!=0 && y!=0) as i32,
            BO::Or =>       (x!=0 || y!=0) as i32,
            BO::Xor =>      ((x!=0) != (y!=0)) as i32,
            BO::BitAnd =>   x&y,
            BO::BitOr =>    x|y,
            BO::BitXor =>   x^y,
            BO::Eq =>       (x==y) as i32,
            BO::Neq =>      (x!=y) as i32,
            BO::Gt =>       (x>y) as i32,
            BO::Ge =>       (x>=y) as i32,
            BO::Lt =>       (x<y) as i32,
            BO::Le =>       (x<=y) as i32,
        })
    }
}

impl UnaryOperator {
    pub fn eval(&self,x:i32) -> i32 {
        match self {
            UO::Minus =>    -x,
            UO::BitNot =>   !x,
            UO::Not =>      (x!=0) as i32,
            UO::Nop =>      x,
        }
    }
}