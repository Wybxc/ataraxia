use crate::fusion::types::Type;

pub trait TypeBuilder {
    fn to(&self, ty: Type) -> Type;
}

impl TypeBuilder for Type {
    fn to(&self, ty: Type) -> Type { Type::function(self.clone(), ty) }
}
