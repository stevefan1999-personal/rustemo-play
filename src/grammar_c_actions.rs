/// This file is maintained by rustemo but can be modified manually.
/// All manual changes will be preserved except non-doc comments.
use rustemo::Token as RustemoToken;
use super::grammar_c::{TokenKind, Context};
pub type Input = str;
pub type Ctx<'i> = Context<'i, Input>;
#[allow(dead_code)]
pub type Token<'i> = RustemoToken<'i, Input, TokenKind>;
pub type Num = String;
pub fn num(_ctx: &Ctx, token: Token) -> Num {
    token.value.into()
}
pub type Identifier = String;
pub fn identifier(_ctx: &Ctx, token: Token) -> Identifier {
    token.value.into()
}
#[derive(Debug, Clone)]
pub struct TranslationUnitC2 {
    pub translation_unit: Box<TranslationUnit>,
    pub external_declaration: ExternalDeclaration,
}
#[derive(Debug, Clone)]
pub enum TranslationUnit {
    ExternalDeclaration(ExternalDeclaration),
    C2(TranslationUnitC2),
}
pub fn translation_unit_external_declaration(
    _ctx: &Ctx,
    external_declaration: ExternalDeclaration,
) -> TranslationUnit {
    TranslationUnit::ExternalDeclaration(external_declaration)
}
pub fn translation_unit_c2(
    _ctx: &Ctx,
    translation_unit: TranslationUnit,
    external_declaration: ExternalDeclaration,
) -> TranslationUnit {
    TranslationUnit::C2(TranslationUnitC2 {
        translation_unit: Box::new(translation_unit),
        external_declaration,
    })
}
#[derive(Debug, Clone)]
pub enum ExternalDeclaration {
    FunctionDefinition(FunctionDefinition),
    Declaration(Declaration),
}
pub fn external_declaration_function_definition(
    _ctx: &Ctx,
    function_definition: FunctionDefinition,
) -> ExternalDeclaration {
    ExternalDeclaration::FunctionDefinition(function_definition)
}
pub fn external_declaration_declaration(
    _ctx: &Ctx,
    declaration: Declaration,
) -> ExternalDeclaration {
    ExternalDeclaration::Declaration(declaration)
}
#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub declaration_specifiers: DeclarationSpecifiers,
    pub declarator: Declarator,
    pub declaration_list_opt: DeclarationListOpt,
    pub compound_statement: CompoundStatement,
}
pub fn function_definition_c1(
    _ctx: &Ctx,
    declaration_specifiers: DeclarationSpecifiers,
    declarator: Declarator,
    declaration_list_opt: DeclarationListOpt,
    compound_statement: CompoundStatement,
) -> FunctionDefinition {
    FunctionDefinition {
        declaration_specifiers,
        declarator,
        declaration_list_opt,
        compound_statement,
    }
}
pub type DeclarationListOpt = Option<DeclarationList>;
pub fn declaration_list_opt_declaration_list(
    _ctx: &Ctx,
    declaration_list: DeclarationList,
) -> DeclarationListOpt {
    Some(declaration_list)
}
pub fn declaration_list_opt_empty(_ctx: &Ctx) -> DeclarationListOpt {
    None
}
#[derive(Debug, Clone)]
pub struct DeclarationSpecifiersC1 {
    pub storage_class_specifier: StorageClassSpecifier,
    pub declaration_specifiers_opt: DeclarationSpecifiersOpt,
}
#[derive(Debug, Clone)]
pub struct DeclarationSpecifiersC2 {
    pub type_specifier: TypeSpecifier,
    pub declaration_specifiers_opt: DeclarationSpecifiersOpt,
}
#[derive(Debug, Clone)]
pub struct DeclarationSpecifiersC3 {
    pub type_qualifier: TypeQualifier,
    pub declaration_specifiers_opt: DeclarationSpecifiersOpt,
}
#[derive(Debug, Clone)]
pub struct DeclarationSpecifiersC4 {
    pub function_specifier: FunctionSpecifier,
    pub declaration_specifiers_opt: DeclarationSpecifiersOpt,
}
#[derive(Debug, Clone)]
pub enum DeclarationSpecifiers {
    C1(DeclarationSpecifiersC1),
    C2(DeclarationSpecifiersC2),
    C3(DeclarationSpecifiersC3),
    C4(DeclarationSpecifiersC4),
}
pub fn declaration_specifiers_c1(
    _ctx: &Ctx,
    storage_class_specifier: StorageClassSpecifier,
    declaration_specifiers_opt: DeclarationSpecifiersOpt,
) -> DeclarationSpecifiers {
    DeclarationSpecifiers::C1(DeclarationSpecifiersC1 {
        storage_class_specifier,
        declaration_specifiers_opt,
    })
}
pub fn declaration_specifiers_c2(
    _ctx: &Ctx,
    type_specifier: TypeSpecifier,
    declaration_specifiers_opt: DeclarationSpecifiersOpt,
) -> DeclarationSpecifiers {
    DeclarationSpecifiers::C2(DeclarationSpecifiersC2 {
        type_specifier,
        declaration_specifiers_opt,
    })
}
pub fn declaration_specifiers_c3(
    _ctx: &Ctx,
    type_qualifier: TypeQualifier,
    declaration_specifiers_opt: DeclarationSpecifiersOpt,
) -> DeclarationSpecifiers {
    DeclarationSpecifiers::C3(DeclarationSpecifiersC3 {
        type_qualifier,
        declaration_specifiers_opt,
    })
}
pub fn declaration_specifiers_c4(
    _ctx: &Ctx,
    function_specifier: FunctionSpecifier,
    declaration_specifiers_opt: DeclarationSpecifiersOpt,
) -> DeclarationSpecifiers {
    DeclarationSpecifiers::C4(DeclarationSpecifiersC4 {
        function_specifier,
        declaration_specifiers_opt,
    })
}
pub type DeclarationSpecifiersOpt = Option<Box<DeclarationSpecifiers>>;
pub fn declaration_specifiers_opt_declaration_specifiers(
    _ctx: &Ctx,
    declaration_specifiers: DeclarationSpecifiers,
) -> DeclarationSpecifiersOpt {
    Some(Box::new(declaration_specifiers))
}
pub fn declaration_specifiers_opt_empty(_ctx: &Ctx) -> DeclarationSpecifiersOpt {
    None
}
#[derive(Debug, Clone)]
pub enum StorageClassSpecifier {
    Typedef,
    Extern,
    Static,
    Auto,
    Register,
}
pub fn storage_class_specifier_typedef(_ctx: &Ctx) -> StorageClassSpecifier {
    StorageClassSpecifier::Typedef
}
pub fn storage_class_specifier_extern(_ctx: &Ctx) -> StorageClassSpecifier {
    StorageClassSpecifier::Extern
}
pub fn storage_class_specifier_static(_ctx: &Ctx) -> StorageClassSpecifier {
    StorageClassSpecifier::Static
}
pub fn storage_class_specifier_auto(_ctx: &Ctx) -> StorageClassSpecifier {
    StorageClassSpecifier::Auto
}
pub fn storage_class_specifier_register(_ctx: &Ctx) -> StorageClassSpecifier {
    StorageClassSpecifier::Register
}
#[derive(Debug, Clone)]
pub enum TypeSpecifier {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    UnderscoreBool,
    UnderscoreComplex,
    StructOrUnionSpecifier(StructOrUnionSpecifier),
    EnumSpecifier(EnumSpecifier),
    TypedefName(TypedefName),
}
pub fn type_specifier_void(_ctx: &Ctx) -> TypeSpecifier {
    TypeSpecifier::Void
}
pub fn type_specifier_char(_ctx: &Ctx) -> TypeSpecifier {
    TypeSpecifier::Char
}
pub fn type_specifier_short(_ctx: &Ctx) -> TypeSpecifier {
    TypeSpecifier::Short
}
pub fn type_specifier_int(_ctx: &Ctx) -> TypeSpecifier {
    TypeSpecifier::Int
}
pub fn type_specifier_long(_ctx: &Ctx) -> TypeSpecifier {
    TypeSpecifier::Long
}
pub fn type_specifier_float(_ctx: &Ctx) -> TypeSpecifier {
    TypeSpecifier::Float
}
pub fn type_specifier_double(_ctx: &Ctx) -> TypeSpecifier {
    TypeSpecifier::Double
}
pub fn type_specifier_signed(_ctx: &Ctx) -> TypeSpecifier {
    TypeSpecifier::Signed
}
pub fn type_specifier_unsigned(_ctx: &Ctx) -> TypeSpecifier {
    TypeSpecifier::Unsigned
}
pub fn type_specifier_underscore_bool(_ctx: &Ctx) -> TypeSpecifier {
    TypeSpecifier::UnderscoreBool
}
pub fn type_specifier_underscore_complex(_ctx: &Ctx) -> TypeSpecifier {
    TypeSpecifier::UnderscoreComplex
}
pub fn type_specifier_struct_or_union_specifier(
    _ctx: &Ctx,
    struct_or_union_specifier: StructOrUnionSpecifier,
) -> TypeSpecifier {
    TypeSpecifier::StructOrUnionSpecifier(struct_or_union_specifier)
}
pub fn type_specifier_enum_specifier(
    _ctx: &Ctx,
    enum_specifier: EnumSpecifier,
) -> TypeSpecifier {
    TypeSpecifier::EnumSpecifier(enum_specifier)
}
pub fn type_specifier_typedef_name(
    _ctx: &Ctx,
    typedef_name: TypedefName,
) -> TypeSpecifier {
    TypeSpecifier::TypedefName(typedef_name)
}
#[derive(Debug, Clone)]
pub struct StructOrUnionSpecifierC1 {
    pub struct_or_union: StructOrUnion,
    pub identifier_opt: IdentifierOpt,
    pub struct_declaration_list: StructDeclarationList,
}
#[derive(Debug, Clone)]
pub struct StructOrUnionSpecifierC2 {
    pub struct_or_union: StructOrUnion,
    pub identifier: Identifier,
}
#[derive(Debug, Clone)]
pub enum StructOrUnionSpecifier {
    C1(StructOrUnionSpecifierC1),
    C2(StructOrUnionSpecifierC2),
}
pub fn struct_or_union_specifier_c1(
    _ctx: &Ctx,
    struct_or_union: StructOrUnion,
    identifier_opt: IdentifierOpt,
    struct_declaration_list: StructDeclarationList,
) -> StructOrUnionSpecifier {
    StructOrUnionSpecifier::C1(StructOrUnionSpecifierC1 {
        struct_or_union,
        identifier_opt,
        struct_declaration_list,
    })
}
pub fn struct_or_union_specifier_c2(
    _ctx: &Ctx,
    struct_or_union: StructOrUnion,
    identifier: Identifier,
) -> StructOrUnionSpecifier {
    StructOrUnionSpecifier::C2(StructOrUnionSpecifierC2 {
        struct_or_union,
        identifier,
    })
}
pub type IdentifierOpt = Option<Identifier>;
pub fn identifier_opt_identifier(_ctx: &Ctx, identifier: Identifier) -> IdentifierOpt {
    Some(identifier)
}
pub fn identifier_opt_empty(_ctx: &Ctx) -> IdentifierOpt {
    None
}
#[derive(Debug, Clone)]
pub enum StructOrUnion {
    Struct,
    Union,
}
pub fn struct_or_union_struct(_ctx: &Ctx) -> StructOrUnion {
    StructOrUnion::Struct
}
pub fn struct_or_union_union(_ctx: &Ctx) -> StructOrUnion {
    StructOrUnion::Union
}
#[derive(Debug, Clone)]
pub struct StructDeclarationListC2 {
    pub struct_declaration_list: Box<StructDeclarationList>,
    pub struct_declaration: StructDeclaration,
}
#[derive(Debug, Clone)]
pub enum StructDeclarationList {
    StructDeclaration(StructDeclaration),
    C2(StructDeclarationListC2),
}
pub fn struct_declaration_list_struct_declaration(
    _ctx: &Ctx,
    struct_declaration: StructDeclaration,
) -> StructDeclarationList {
    StructDeclarationList::StructDeclaration(struct_declaration)
}
pub fn struct_declaration_list_c2(
    _ctx: &Ctx,
    struct_declaration_list: StructDeclarationList,
    struct_declaration: StructDeclaration,
) -> StructDeclarationList {
    StructDeclarationList::C2(StructDeclarationListC2 {
        struct_declaration_list: Box::new(struct_declaration_list),
        struct_declaration,
    })
}
#[derive(Debug, Clone)]
pub struct StructDeclaration {
    pub specifier_qualifier_list: SpecifierQualifierList,
    pub struct_declarator_list: StructDeclaratorList,
}
pub fn struct_declaration_c1(
    _ctx: &Ctx,
    specifier_qualifier_list: SpecifierQualifierList,
    struct_declarator_list: StructDeclaratorList,
) -> StructDeclaration {
    StructDeclaration {
        specifier_qualifier_list,
        struct_declarator_list,
    }
}
#[derive(Debug, Clone)]
pub struct SpecifierQualifierListC1 {
    pub type_specifier: Box<TypeSpecifier>,
    pub specifier_qualifier_list_opt: SpecifierQualifierListOpt,
}
#[derive(Debug, Clone)]
pub struct SpecifierQualifierListC2 {
    pub type_qualifier: TypeQualifier,
    pub specifier_qualifier_list_opt: SpecifierQualifierListOpt,
}
#[derive(Debug, Clone)]
pub enum SpecifierQualifierList {
    C1(SpecifierQualifierListC1),
    C2(SpecifierQualifierListC2),
}
pub fn specifier_qualifier_list_c1(
    _ctx: &Ctx,
    type_specifier: TypeSpecifier,
    specifier_qualifier_list_opt: SpecifierQualifierListOpt,
) -> SpecifierQualifierList {
    SpecifierQualifierList::C1(SpecifierQualifierListC1 {
        type_specifier: Box::new(type_specifier),
        specifier_qualifier_list_opt,
    })
}
pub fn specifier_qualifier_list_c2(
    _ctx: &Ctx,
    type_qualifier: TypeQualifier,
    specifier_qualifier_list_opt: SpecifierQualifierListOpt,
) -> SpecifierQualifierList {
    SpecifierQualifierList::C2(SpecifierQualifierListC2 {
        type_qualifier,
        specifier_qualifier_list_opt,
    })
}
pub type SpecifierQualifierListOpt = Option<Box<SpecifierQualifierList>>;
pub fn specifier_qualifier_list_opt_specifier_qualifier_list(
    _ctx: &Ctx,
    specifier_qualifier_list: SpecifierQualifierList,
) -> SpecifierQualifierListOpt {
    Some(Box::new(specifier_qualifier_list))
}
pub fn specifier_qualifier_list_opt_empty(_ctx: &Ctx) -> SpecifierQualifierListOpt {
    None
}
#[derive(Debug, Clone)]
pub enum TypeQualifier {
    Const,
    Restrict,
    Volatile,
}
pub fn type_qualifier_const(_ctx: &Ctx) -> TypeQualifier {
    TypeQualifier::Const
}
pub fn type_qualifier_restrict(_ctx: &Ctx) -> TypeQualifier {
    TypeQualifier::Restrict
}
pub fn type_qualifier_volatile(_ctx: &Ctx) -> TypeQualifier {
    TypeQualifier::Volatile
}
#[derive(Debug, Clone)]
pub struct StructDeclaratorListC2 {
    pub struct_declarator_list: Box<StructDeclaratorList>,
    pub struct_declarator: StructDeclarator,
}
#[derive(Debug, Clone)]
pub enum StructDeclaratorList {
    StructDeclarator(StructDeclarator),
    C2(StructDeclaratorListC2),
}
pub fn struct_declarator_list_struct_declarator(
    _ctx: &Ctx,
    struct_declarator: StructDeclarator,
) -> StructDeclaratorList {
    StructDeclaratorList::StructDeclarator(struct_declarator)
}
pub fn struct_declarator_list_c2(
    _ctx: &Ctx,
    struct_declarator_list: StructDeclaratorList,
    struct_declarator: StructDeclarator,
) -> StructDeclaratorList {
    StructDeclaratorList::C2(StructDeclaratorListC2 {
        struct_declarator_list: Box::new(struct_declarator_list),
        struct_declarator,
    })
}
#[derive(Debug, Clone)]
pub struct StructDeclaratorC2 {
    pub declarator_opt: DeclaratorOpt,
    pub constant_expression: ConstantExpression,
}
#[derive(Debug, Clone)]
pub enum StructDeclarator {
    Declarator(Declarator),
    C2(StructDeclaratorC2),
}
pub fn struct_declarator_declarator(
    _ctx: &Ctx,
    declarator: Declarator,
) -> StructDeclarator {
    StructDeclarator::Declarator(declarator)
}
pub fn struct_declarator_c2(
    _ctx: &Ctx,
    declarator_opt: DeclaratorOpt,
    constant_expression: ConstantExpression,
) -> StructDeclarator {
    StructDeclarator::C2(StructDeclaratorC2 {
        declarator_opt,
        constant_expression,
    })
}
pub type DeclaratorOpt = Option<Declarator>;
pub fn declarator_opt_declarator(_ctx: &Ctx, declarator: Declarator) -> DeclaratorOpt {
    Some(declarator)
}
pub fn declarator_opt_empty(_ctx: &Ctx) -> DeclaratorOpt {
    None
}
#[derive(Debug, Clone)]
pub struct Declarator {
    pub pointer_opt: PointerOpt,
    pub direct_declarator: DirectDeclarator,
}
pub fn declarator_c1(
    _ctx: &Ctx,
    pointer_opt: PointerOpt,
    direct_declarator: DirectDeclarator,
) -> Declarator {
    Declarator {
        pointer_opt,
        direct_declarator,
    }
}
pub type PointerOpt = Option<Pointer>;
pub fn pointer_opt_pointer(_ctx: &Ctx, pointer: Pointer) -> PointerOpt {
    Some(pointer)
}
pub fn pointer_opt_empty(_ctx: &Ctx) -> PointerOpt {
    None
}
#[derive(Debug, Clone)]
pub struct PointerC2 {
    pub type_qualifier_list_opt: TypeQualifierListOpt,
    pub pointer: Box<Pointer>,
}
#[derive(Debug, Clone)]
pub enum Pointer {
    TypeQualifierListOpt(TypeQualifierListOpt),
    C2(PointerC2),
}
pub fn pointer_type_qualifier_list_opt(
    _ctx: &Ctx,
    type_qualifier_list_opt: TypeQualifierListOpt,
) -> Pointer {
    Pointer::TypeQualifierListOpt(type_qualifier_list_opt)
}
pub fn pointer_c2(
    _ctx: &Ctx,
    type_qualifier_list_opt: TypeQualifierListOpt,
    pointer: Pointer,
) -> Pointer {
    Pointer::C2(PointerC2 {
        type_qualifier_list_opt,
        pointer: Box::new(pointer),
    })
}
pub type TypeQualifierListOpt = Option<TypeQualifierList>;
pub fn type_qualifier_list_opt_type_qualifier_list(
    _ctx: &Ctx,
    type_qualifier_list: TypeQualifierList,
) -> TypeQualifierListOpt {
    Some(type_qualifier_list)
}
pub fn type_qualifier_list_opt_empty(_ctx: &Ctx) -> TypeQualifierListOpt {
    None
}
#[derive(Debug, Clone)]
pub struct TypeQualifierListC2 {
    pub type_qualifier_list: Box<TypeQualifierList>,
    pub type_qualifier: TypeQualifier,
}
#[derive(Debug, Clone)]
pub enum TypeQualifierList {
    TypeQualifier(TypeQualifier),
    C2(TypeQualifierListC2),
}
pub fn type_qualifier_list_type_qualifier(
    _ctx: &Ctx,
    type_qualifier: TypeQualifier,
) -> TypeQualifierList {
    TypeQualifierList::TypeQualifier(type_qualifier)
}
pub fn type_qualifier_list_c2(
    _ctx: &Ctx,
    type_qualifier_list: TypeQualifierList,
    type_qualifier: TypeQualifier,
) -> TypeQualifierList {
    TypeQualifierList::C2(TypeQualifierListC2 {
        type_qualifier_list: Box::new(type_qualifier_list),
        type_qualifier,
    })
}
#[derive(Debug, Clone)]
pub struct DirectDeclaratorC3 {
    pub direct_declarator: Box<DirectDeclarator>,
    pub type_qualifier_list_opt: TypeQualifierListOpt,
    pub assignment_expression_opt: AssignmentExpressionOpt,
}
#[derive(Debug, Clone)]
pub struct DirectDeclaratorC4 {
    pub direct_declarator: Box<DirectDeclarator>,
    pub type_qualifier_list_opt: TypeQualifierListOpt,
    pub assignment_expression: AssignmentExpression,
}
#[derive(Debug, Clone)]
pub struct DirectDeclaratorC5 {
    pub direct_declarator: Box<DirectDeclarator>,
    pub type_qualifier_list: TypeQualifierList,
    pub assignment_expression: AssignmentExpression,
}
#[derive(Debug, Clone)]
pub struct DirectDeclaratorC6 {
    pub direct_declarator: Box<DirectDeclarator>,
    pub type_qualifier_list_opt: TypeQualifierListOpt,
}
#[derive(Debug, Clone)]
pub struct DirectDeclaratorC7 {
    pub direct_declarator: Box<DirectDeclarator>,
    pub parameter_type_list: ParameterTypeList,
}
#[derive(Debug, Clone)]
pub struct DirectDeclaratorC8 {
    pub direct_declarator: Box<DirectDeclarator>,
    pub identifier_list_opt: IdentifierListOpt,
}
#[derive(Debug, Clone)]
pub enum DirectDeclarator {
    Identifier(Identifier),
    Declarator(Box<Declarator>),
    C3(DirectDeclaratorC3),
    C4(DirectDeclaratorC4),
    C5(DirectDeclaratorC5),
    C6(DirectDeclaratorC6),
    C7(DirectDeclaratorC7),
    C8(DirectDeclaratorC8),
}
pub fn direct_declarator_identifier(
    _ctx: &Ctx,
    identifier: Identifier,
) -> DirectDeclarator {
    DirectDeclarator::Identifier(identifier)
}
pub fn direct_declarator_declarator(
    _ctx: &Ctx,
    declarator: Declarator,
) -> DirectDeclarator {
    DirectDeclarator::Declarator(Box::new(declarator))
}
pub fn direct_declarator_c3(
    _ctx: &Ctx,
    direct_declarator: DirectDeclarator,
    type_qualifier_list_opt: TypeQualifierListOpt,
    assignment_expression_opt: AssignmentExpressionOpt,
) -> DirectDeclarator {
    DirectDeclarator::C3(DirectDeclaratorC3 {
        direct_declarator: Box::new(direct_declarator),
        type_qualifier_list_opt,
        assignment_expression_opt,
    })
}
pub fn direct_declarator_c4(
    _ctx: &Ctx,
    direct_declarator: DirectDeclarator,
    type_qualifier_list_opt: TypeQualifierListOpt,
    assignment_expression: AssignmentExpression,
) -> DirectDeclarator {
    DirectDeclarator::C4(DirectDeclaratorC4 {
        direct_declarator: Box::new(direct_declarator),
        type_qualifier_list_opt,
        assignment_expression,
    })
}
pub fn direct_declarator_c5(
    _ctx: &Ctx,
    direct_declarator: DirectDeclarator,
    type_qualifier_list: TypeQualifierList,
    assignment_expression: AssignmentExpression,
) -> DirectDeclarator {
    DirectDeclarator::C5(DirectDeclaratorC5 {
        direct_declarator: Box::new(direct_declarator),
        type_qualifier_list,
        assignment_expression,
    })
}
pub fn direct_declarator_c6(
    _ctx: &Ctx,
    direct_declarator: DirectDeclarator,
    type_qualifier_list_opt: TypeQualifierListOpt,
) -> DirectDeclarator {
    DirectDeclarator::C6(DirectDeclaratorC6 {
        direct_declarator: Box::new(direct_declarator),
        type_qualifier_list_opt,
    })
}
pub fn direct_declarator_c7(
    _ctx: &Ctx,
    direct_declarator: DirectDeclarator,
    parameter_type_list: ParameterTypeList,
) -> DirectDeclarator {
    DirectDeclarator::C7(DirectDeclaratorC7 {
        direct_declarator: Box::new(direct_declarator),
        parameter_type_list,
    })
}
pub fn direct_declarator_c8(
    _ctx: &Ctx,
    direct_declarator: DirectDeclarator,
    identifier_list_opt: IdentifierListOpt,
) -> DirectDeclarator {
    DirectDeclarator::C8(DirectDeclaratorC8 {
        direct_declarator: Box::new(direct_declarator),
        identifier_list_opt,
    })
}
pub type AssignmentExpressionOpt = Option<AssignmentExpression>;
pub fn assignment_expression_opt_assignment_expression(
    _ctx: &Ctx,
    assignment_expression: AssignmentExpression,
) -> AssignmentExpressionOpt {
    Some(assignment_expression)
}
pub fn assignment_expression_opt_empty(_ctx: &Ctx) -> AssignmentExpressionOpt {
    None
}
pub type IdentifierListOpt = Option<IdentifierList>;
pub fn identifier_list_opt_identifier_list(
    _ctx: &Ctx,
    identifier_list: IdentifierList,
) -> IdentifierListOpt {
    Some(identifier_list)
}
pub fn identifier_list_opt_empty(_ctx: &Ctx) -> IdentifierListOpt {
    None
}
#[derive(Debug, Clone)]
pub struct AssignmentExpression {
    pub left: Expression,
    pub right: Expression,
}
pub fn assignment_expression_c1(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> AssignmentExpression {
    AssignmentExpression {
        left,
        right,
    }
}
#[derive(Debug, Clone)]
pub struct ConditionalExpressionC2 {
    pub condition: ArithmeticExpression,
    pub branch_true: Box<Expression>,
    pub branch_false: Box<ConditionalExpression>,
}
#[derive(Debug, Clone)]
pub enum ConditionalExpression {
    ArithmeticExpression(ArithmeticExpression),
    C2(ConditionalExpressionC2),
}
pub fn conditional_expression_arithmetic_expression(
    _ctx: &Ctx,
    arithmetic_expression: ArithmeticExpression,
) -> ConditionalExpression {
    ConditionalExpression::ArithmeticExpression(arithmetic_expression)
}
pub fn conditional_expression_c2(
    _ctx: &Ctx,
    condition: ArithmeticExpression,
    branch_true: Expression,
    branch_false: ConditionalExpression,
) -> ConditionalExpression {
    ConditionalExpression::C2(ConditionalExpressionC2 {
        condition,
        branch_true: Box::new(branch_true),
        branch_false: Box::new(branch_false),
    })
}
#[derive(Debug, Clone)]
pub struct LogicalOrExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}
#[derive(Debug, Clone)]
pub struct LogicalAndExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}
#[derive(Debug, Clone)]
pub struct BitwiseOrExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}
#[derive(Debug, Clone)]
pub struct BitwiseExclusiveOrExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}
#[derive(Debug, Clone)]
pub struct BitwiseAndExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}
#[derive(Debug, Clone)]
pub struct LogicalEqualExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}
#[derive(Debug, Clone)]
pub struct LogicalNotEqualExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}
#[derive(Debug, Clone)]
pub struct LessThanExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}
#[derive(Debug, Clone)]
pub struct LessThanOrEqualExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}
#[derive(Debug, Clone)]
pub struct GreaterThanExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}
#[derive(Debug, Clone)]
pub struct GreaterThanOrEqualExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}
#[derive(Debug, Clone)]
pub struct BitwiseLeftShiftExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}
#[derive(Debug, Clone)]
pub struct BitwiseRightShiftExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}
#[derive(Debug, Clone)]
pub struct AdditionExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}
#[derive(Debug, Clone)]
pub struct SubtractionExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}
#[derive(Debug, Clone)]
pub struct MultiplyExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}
#[derive(Debug, Clone)]
pub struct DivisionExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}
#[derive(Debug, Clone)]
pub struct ModuloExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}
#[derive(Debug, Clone)]
pub enum ArithmeticExpression {
    LogicalOrExpression(LogicalOrExpression),
    LogicalAndExpression(LogicalAndExpression),
    BitwiseOrExpression(BitwiseOrExpression),
    BitwiseExclusiveOrExpression(BitwiseExclusiveOrExpression),
    BitwiseAndExpression(BitwiseAndExpression),
    LogicalEqualExpression(LogicalEqualExpression),
    LogicalNotEqualExpression(LogicalNotEqualExpression),
    LessThanExpression(LessThanExpression),
    LessThanOrEqualExpression(LessThanOrEqualExpression),
    GreaterThanExpression(GreaterThanExpression),
    GreaterThanOrEqualExpression(GreaterThanOrEqualExpression),
    BitwiseLeftShiftExpression(BitwiseLeftShiftExpression),
    BitwiseRightShiftExpression(BitwiseRightShiftExpression),
    AdditionExpression(AdditionExpression),
    SubtractionExpression(SubtractionExpression),
    MultiplyExpression(MultiplyExpression),
    DivisionExpression(DivisionExpression),
    ModuloExpression(ModuloExpression),
}
pub fn arithmetic_expression_logical_or_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::LogicalOrExpression(LogicalOrExpression {
        left: Box::new(left),
        right: Box::new(right),
    })
}
pub fn arithmetic_expression_logical_and_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::LogicalAndExpression(LogicalAndExpression {
        left: Box::new(left),
        right: Box::new(right),
    })
}
pub fn arithmetic_expression_bitwise_or_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::BitwiseOrExpression(BitwiseOrExpression {
        left: Box::new(left),
        right: Box::new(right),
    })
}
pub fn arithmetic_expression_bitwise_exclusive_or_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::BitwiseExclusiveOrExpression(BitwiseExclusiveOrExpression {
        left: Box::new(left),
        right: Box::new(right),
    })
}
pub fn arithmetic_expression_bitwise_and_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::BitwiseAndExpression(BitwiseAndExpression {
        left: Box::new(left),
        right: Box::new(right),
    })
}
pub fn arithmetic_expression_logical_equal_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::LogicalEqualExpression(LogicalEqualExpression {
        left: Box::new(left),
        right: Box::new(right),
    })
}
pub fn arithmetic_expression_logical_not_equal_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::LogicalNotEqualExpression(LogicalNotEqualExpression {
        left: Box::new(left),
        right: Box::new(right),
    })
}
pub fn arithmetic_expression_less_than_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::LessThanExpression(LessThanExpression {
        left: Box::new(left),
        right: Box::new(right),
    })
}
pub fn arithmetic_expression_less_than_or_equal_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::LessThanOrEqualExpression(LessThanOrEqualExpression {
        left: Box::new(left),
        right: Box::new(right),
    })
}
pub fn arithmetic_expression_greater_than_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::GreaterThanExpression(GreaterThanExpression {
        left: Box::new(left),
        right: Box::new(right),
    })
}
pub fn arithmetic_expression_greater_than_or_equal_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::GreaterThanOrEqualExpression(GreaterThanOrEqualExpression {
        left: Box::new(left),
        right: Box::new(right),
    })
}
pub fn arithmetic_expression_bitwise_left_shift_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::BitwiseLeftShiftExpression(BitwiseLeftShiftExpression {
        left: Box::new(left),
        right: Box::new(right),
    })
}
pub fn arithmetic_expression_bitwise_right_shift_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::BitwiseRightShiftExpression(BitwiseRightShiftExpression {
        left: Box::new(left),
        right: Box::new(right),
    })
}
pub fn arithmetic_expression_addition_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::AdditionExpression(AdditionExpression {
        left: Box::new(left),
        right: Box::new(right),
    })
}
pub fn arithmetic_expression_subtraction_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::SubtractionExpression(SubtractionExpression {
        left: Box::new(left),
        right: Box::new(right),
    })
}
pub fn arithmetic_expression_multiply_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::MultiplyExpression(MultiplyExpression {
        left: Box::new(left),
        right: Box::new(right),
    })
}
pub fn arithmetic_expression_division_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::DivisionExpression(DivisionExpression {
        left: Box::new(left),
        right: Box::new(right),
    })
}
pub fn arithmetic_expression_modulo_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::ModuloExpression(ModuloExpression {
        left: Box::new(left),
        right: Box::new(right),
    })
}
#[derive(Debug, Clone)]
pub struct CastExpressionC2 {
    pub type_name: TypeName,
    pub cast_expression: Box<CastExpression>,
}
#[derive(Debug, Clone)]
pub enum CastExpression {
    UnaryExpression(Box<UnaryExpression>),
    C2(CastExpressionC2),
}
pub fn cast_expression_unary_expression(
    _ctx: &Ctx,
    unary_expression: UnaryExpression,
) -> CastExpression {
    CastExpression::UnaryExpression(Box::new(unary_expression))
}
pub fn cast_expression_c2(
    _ctx: &Ctx,
    type_name: TypeName,
    cast_expression: CastExpression,
) -> CastExpression {
    CastExpression::C2(CastExpressionC2 {
        type_name,
        cast_expression: Box::new(cast_expression),
    })
}
#[derive(Debug, Clone)]
pub enum UnaryExpression {
    PostfixExpression(PostfixExpression),
    PreIncrementExpression(Box<UnaryExpression>),
    PreDecrementExpression(Box<UnaryExpression>),
    LogicalNotExpression(CastExpression),
    BitwiseNotExpression(CastExpression),
    PlusExpression(CastExpression),
    MinusExpression(CastExpression),
    DereferenceExpression(CastExpression),
    ReferenceExpression(CastExpression),
    SizeOfExpressionExpression(Box<Expression>),
    SizeOfTypenameExpression(TypeName),
}
pub fn unary_expression_postfix_expression(
    _ctx: &Ctx,
    postfix_expression: PostfixExpression,
) -> UnaryExpression {
    UnaryExpression::PostfixExpression(postfix_expression)
}
pub fn unary_expression_pre_increment_expression(
    _ctx: &Ctx,
    unary_expression: UnaryExpression,
) -> UnaryExpression {
    UnaryExpression::PreIncrementExpression(Box::new(unary_expression))
}
pub fn unary_expression_pre_decrement_expression(
    _ctx: &Ctx,
    unary_expression: UnaryExpression,
) -> UnaryExpression {
    UnaryExpression::PreDecrementExpression(Box::new(unary_expression))
}
pub fn unary_expression_logical_not_expression(
    _ctx: &Ctx,
    cast_expression: CastExpression,
) -> UnaryExpression {
    UnaryExpression::LogicalNotExpression(cast_expression)
}
pub fn unary_expression_bitwise_not_expression(
    _ctx: &Ctx,
    cast_expression: CastExpression,
) -> UnaryExpression {
    UnaryExpression::BitwiseNotExpression(cast_expression)
}
pub fn unary_expression_plus_expression(
    _ctx: &Ctx,
    cast_expression: CastExpression,
) -> UnaryExpression {
    UnaryExpression::PlusExpression(cast_expression)
}
pub fn unary_expression_minus_expression(
    _ctx: &Ctx,
    cast_expression: CastExpression,
) -> UnaryExpression {
    UnaryExpression::MinusExpression(cast_expression)
}
pub fn unary_expression_dereference_expression(
    _ctx: &Ctx,
    cast_expression: CastExpression,
) -> UnaryExpression {
    UnaryExpression::DereferenceExpression(cast_expression)
}
pub fn unary_expression_reference_expression(
    _ctx: &Ctx,
    cast_expression: CastExpression,
) -> UnaryExpression {
    UnaryExpression::ReferenceExpression(cast_expression)
}
pub fn unary_expression_size_of_expression_expression(
    _ctx: &Ctx,
    expression: Expression,
) -> UnaryExpression {
    UnaryExpression::SizeOfExpressionExpression(Box::new(expression))
}
pub fn unary_expression_size_of_typename_expression(
    _ctx: &Ctx,
    type_name: TypeName,
) -> UnaryExpression {
    UnaryExpression::SizeOfTypenameExpression(type_name)
}
#[derive(Debug, Clone)]
pub struct PostfixExpressionC2 {
    pub postfix_expression: Box<PostfixExpression>,
    pub expression: Box<Expression>,
}
#[derive(Debug, Clone)]
pub struct PostfixExpressionC3 {
    pub postfix_expression: Box<PostfixExpression>,
    pub argument_expression_list_opt: ArgumentExpressionListOpt,
}
#[derive(Debug, Clone)]
pub struct PostfixExpressionC4 {
    pub postfix_expression: Box<PostfixExpression>,
    pub identifier: Identifier,
}
#[derive(Debug, Clone)]
pub struct PostfixExpressionC5 {
    pub postfix_expression: Box<PostfixExpression>,
    pub identifier: Identifier,
}
#[derive(Debug, Clone)]
pub struct PostfixExpressionC8 {
    pub type_name: TypeName,
    pub initializer_list: InitializerList,
}
#[derive(Debug, Clone)]
pub struct PostfixExpressionC9 {
    pub type_name: TypeName,
    pub initializer_list: InitializerList,
}
#[derive(Debug, Clone)]
pub enum PostfixExpression {
    PrimaryExpression(PrimaryExpression),
    C2(PostfixExpressionC2),
    C3(PostfixExpressionC3),
    C4(PostfixExpressionC4),
    C5(PostfixExpressionC5),
    PostfixExpression1(Box<PostfixExpression>),
    PostfixExpression2(Box<PostfixExpression>),
    C8(PostfixExpressionC8),
    C9(PostfixExpressionC9),
}
pub fn postfix_expression_primary_expression(
    _ctx: &Ctx,
    primary_expression: PrimaryExpression,
) -> PostfixExpression {
    PostfixExpression::PrimaryExpression(primary_expression)
}
pub fn postfix_expression_c2(
    _ctx: &Ctx,
    postfix_expression: PostfixExpression,
    expression: Expression,
) -> PostfixExpression {
    PostfixExpression::C2(PostfixExpressionC2 {
        postfix_expression: Box::new(postfix_expression),
        expression: Box::new(expression),
    })
}
pub fn postfix_expression_c3(
    _ctx: &Ctx,
    postfix_expression: PostfixExpression,
    argument_expression_list_opt: ArgumentExpressionListOpt,
) -> PostfixExpression {
    PostfixExpression::C3(PostfixExpressionC3 {
        postfix_expression: Box::new(postfix_expression),
        argument_expression_list_opt,
    })
}
pub fn postfix_expression_c4(
    _ctx: &Ctx,
    postfix_expression: PostfixExpression,
    identifier: Identifier,
) -> PostfixExpression {
    PostfixExpression::C4(PostfixExpressionC4 {
        postfix_expression: Box::new(postfix_expression),
        identifier,
    })
}
pub fn postfix_expression_c5(
    _ctx: &Ctx,
    postfix_expression: PostfixExpression,
    identifier: Identifier,
) -> PostfixExpression {
    PostfixExpression::C5(PostfixExpressionC5 {
        postfix_expression: Box::new(postfix_expression),
        identifier,
    })
}
pub fn postfix_expression_postfix_expression1(
    _ctx: &Ctx,
    postfix_expression: PostfixExpression,
) -> PostfixExpression {
    PostfixExpression::PostfixExpression1(Box::new(postfix_expression))
}
pub fn postfix_expression_postfix_expression2(
    _ctx: &Ctx,
    postfix_expression: PostfixExpression,
) -> PostfixExpression {
    PostfixExpression::PostfixExpression2(Box::new(postfix_expression))
}
pub fn postfix_expression_c8(
    _ctx: &Ctx,
    type_name: TypeName,
    initializer_list: InitializerList,
) -> PostfixExpression {
    PostfixExpression::C8(PostfixExpressionC8 {
        type_name,
        initializer_list,
    })
}
pub fn postfix_expression_c9(
    _ctx: &Ctx,
    type_name: TypeName,
    initializer_list: InitializerList,
) -> PostfixExpression {
    PostfixExpression::C9(PostfixExpressionC9 {
        type_name,
        initializer_list,
    })
}
pub type ArgumentExpressionListOpt = Option<ArgumentExpressionList>;
pub fn argument_expression_list_opt_argument_expression_list(
    _ctx: &Ctx,
    argument_expression_list: ArgumentExpressionList,
) -> ArgumentExpressionListOpt {
    Some(argument_expression_list)
}
pub fn argument_expression_list_opt_empty(_ctx: &Ctx) -> ArgumentExpressionListOpt {
    None
}
#[derive(Debug, Clone)]
pub enum PrimaryExpression {
    Number(Num),
    Identifier(Identifier),
    Paren(Box<Expression>),
}
pub fn primary_expression_number(_ctx: &Ctx, num: Num) -> PrimaryExpression {
    PrimaryExpression::Number(num)
}
pub fn primary_expression_identifier(
    _ctx: &Ctx,
    identifier: Identifier,
) -> PrimaryExpression {
    PrimaryExpression::Identifier(identifier)
}
pub fn primary_expression_paren(
    _ctx: &Ctx,
    expression: Expression,
) -> PrimaryExpression {
    PrimaryExpression::Paren(Box::new(expression))
}
#[derive(Debug, Clone)]
pub struct CommaExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}
#[derive(Debug, Clone)]
pub enum Expression {
    CommaExpression(CommaExpression),
    AssignmentExpression(Box<AssignmentExpression>),
    ConditionalExpression(ConditionalExpression),
    ArithmeticExpression(ArithmeticExpression),
    UnaryExpression(UnaryExpression),
}
pub fn expression_comma_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> Expression {
    Expression::CommaExpression(CommaExpression {
        left: Box::new(left),
        right: Box::new(right),
    })
}
pub fn expression_assignment_expression(
    _ctx: &Ctx,
    assignment_expression: AssignmentExpression,
) -> Expression {
    Expression::AssignmentExpression(Box::new(assignment_expression))
}
pub fn expression_conditional_expression(
    _ctx: &Ctx,
    conditional_expression: ConditionalExpression,
) -> Expression {
    Expression::ConditionalExpression(conditional_expression)
}
pub fn expression_arithmetic_expression(
    _ctx: &Ctx,
    arithmetic_expression: ArithmeticExpression,
) -> Expression {
    Expression::ArithmeticExpression(arithmetic_expression)
}
pub fn expression_unary_expression(
    _ctx: &Ctx,
    unary_expression: UnaryExpression,
) -> Expression {
    Expression::UnaryExpression(unary_expression)
}
#[derive(Debug, Clone)]
pub struct ArgumentExpressionListC2 {
    pub argument_expression_list: Box<ArgumentExpressionList>,
    pub assignment_expression: Box<AssignmentExpression>,
}
#[derive(Debug, Clone)]
pub enum ArgumentExpressionList {
    AssignmentExpression(Box<AssignmentExpression>),
    C2(ArgumentExpressionListC2),
}
pub fn argument_expression_list_assignment_expression(
    _ctx: &Ctx,
    assignment_expression: AssignmentExpression,
) -> ArgumentExpressionList {
    ArgumentExpressionList::AssignmentExpression(Box::new(assignment_expression))
}
pub fn argument_expression_list_c2(
    _ctx: &Ctx,
    argument_expression_list: ArgumentExpressionList,
    assignment_expression: AssignmentExpression,
) -> ArgumentExpressionList {
    ArgumentExpressionList::C2(ArgumentExpressionListC2 {
        argument_expression_list: Box::new(argument_expression_list),
        assignment_expression: Box::new(assignment_expression),
    })
}
#[derive(Debug, Clone)]
pub struct TypeName {
    pub specifier_qualifier_list: SpecifierQualifierList,
    pub abstract_declarator_opt: AbstractDeclaratorOpt,
}
pub fn type_name_c1(
    _ctx: &Ctx,
    specifier_qualifier_list: SpecifierQualifierList,
    abstract_declarator_opt: AbstractDeclaratorOpt,
) -> TypeName {
    TypeName {
        specifier_qualifier_list,
        abstract_declarator_opt,
    }
}
pub type AbstractDeclaratorOpt = Option<AbstractDeclarator>;
pub fn abstract_declarator_opt_abstract_declarator(
    _ctx: &Ctx,
    abstract_declarator: AbstractDeclarator,
) -> AbstractDeclaratorOpt {
    Some(abstract_declarator)
}
pub fn abstract_declarator_opt_empty(_ctx: &Ctx) -> AbstractDeclaratorOpt {
    None
}
#[derive(Debug, Clone)]
pub struct AbstractDeclaratorC2 {
    pub pointer_opt: PointerOpt,
    pub direct_abstract_declarator: DirectAbstractDeclarator,
}
#[derive(Debug, Clone)]
pub enum AbstractDeclarator {
    Pointer(Pointer),
    C2(AbstractDeclaratorC2),
}
pub fn abstract_declarator_pointer(_ctx: &Ctx, pointer: Pointer) -> AbstractDeclarator {
    AbstractDeclarator::Pointer(pointer)
}
pub fn abstract_declarator_c2(
    _ctx: &Ctx,
    pointer_opt: PointerOpt,
    direct_abstract_declarator: DirectAbstractDeclarator,
) -> AbstractDeclarator {
    AbstractDeclarator::C2(AbstractDeclaratorC2 {
        pointer_opt,
        direct_abstract_declarator,
    })
}
#[derive(Debug, Clone)]
pub struct DirectAbstractDeclaratorC2 {
    pub direct_abstract_declarator_opt: DirectAbstractDeclaratorOpt,
    pub type_qualifier_list_opt: TypeQualifierListOpt,
    pub assignment_expression_opt: Box<AssignmentExpressionOpt>,
}
#[derive(Debug, Clone)]
pub struct DirectAbstractDeclaratorC3 {
    pub direct_abstract_declarator_opt: DirectAbstractDeclaratorOpt,
    pub type_qualifier_list_opt: TypeQualifierListOpt,
    pub assignment_expression: Box<AssignmentExpression>,
}
#[derive(Debug, Clone)]
pub struct DirectAbstractDeclaratorC4 {
    pub direct_abstract_declarator_opt: DirectAbstractDeclaratorOpt,
    pub type_qualifier_list: TypeQualifierList,
    pub assignment_expression: Box<AssignmentExpression>,
}
#[derive(Debug, Clone)]
pub struct DirectAbstractDeclaratorC6 {
    pub direct_abstract_declarator_opt: DirectAbstractDeclaratorOpt,
    pub parameter_type_list_opt: ParameterTypeListOpt,
}
#[derive(Debug, Clone)]
pub enum DirectAbstractDeclarator {
    AbstractDeclarator(Box<AbstractDeclarator>),
    C2(DirectAbstractDeclaratorC2),
    C3(DirectAbstractDeclaratorC3),
    C4(DirectAbstractDeclaratorC4),
    DirectAbstractDeclaratorOpt(DirectAbstractDeclaratorOpt),
    C6(DirectAbstractDeclaratorC6),
}
pub fn direct_abstract_declarator_abstract_declarator(
    _ctx: &Ctx,
    abstract_declarator: AbstractDeclarator,
) -> DirectAbstractDeclarator {
    DirectAbstractDeclarator::AbstractDeclarator(Box::new(abstract_declarator))
}
pub fn direct_abstract_declarator_c2(
    _ctx: &Ctx,
    direct_abstract_declarator_opt: DirectAbstractDeclaratorOpt,
    type_qualifier_list_opt: TypeQualifierListOpt,
    assignment_expression_opt: AssignmentExpressionOpt,
) -> DirectAbstractDeclarator {
    DirectAbstractDeclarator::C2(DirectAbstractDeclaratorC2 {
        direct_abstract_declarator_opt,
        type_qualifier_list_opt,
        assignment_expression_opt: Box::new(assignment_expression_opt),
    })
}
pub fn direct_abstract_declarator_c3(
    _ctx: &Ctx,
    direct_abstract_declarator_opt: DirectAbstractDeclaratorOpt,
    type_qualifier_list_opt: TypeQualifierListOpt,
    assignment_expression: AssignmentExpression,
) -> DirectAbstractDeclarator {
    DirectAbstractDeclarator::C3(DirectAbstractDeclaratorC3 {
        direct_abstract_declarator_opt,
        type_qualifier_list_opt,
        assignment_expression: Box::new(assignment_expression),
    })
}
pub fn direct_abstract_declarator_c4(
    _ctx: &Ctx,
    direct_abstract_declarator_opt: DirectAbstractDeclaratorOpt,
    type_qualifier_list: TypeQualifierList,
    assignment_expression: AssignmentExpression,
) -> DirectAbstractDeclarator {
    DirectAbstractDeclarator::C4(DirectAbstractDeclaratorC4 {
        direct_abstract_declarator_opt,
        type_qualifier_list,
        assignment_expression: Box::new(assignment_expression),
    })
}
pub fn direct_abstract_declarator_direct_abstract_declarator_opt(
    _ctx: &Ctx,
    direct_abstract_declarator_opt: DirectAbstractDeclaratorOpt,
) -> DirectAbstractDeclarator {
    DirectAbstractDeclarator::DirectAbstractDeclaratorOpt(direct_abstract_declarator_opt)
}
pub fn direct_abstract_declarator_c6(
    _ctx: &Ctx,
    direct_abstract_declarator_opt: DirectAbstractDeclaratorOpt,
    parameter_type_list_opt: ParameterTypeListOpt,
) -> DirectAbstractDeclarator {
    DirectAbstractDeclarator::C6(DirectAbstractDeclaratorC6 {
        direct_abstract_declarator_opt,
        parameter_type_list_opt,
    })
}
pub type DirectAbstractDeclaratorOpt = Option<Box<DirectAbstractDeclarator>>;
pub fn direct_abstract_declarator_opt_direct_abstract_declarator(
    _ctx: &Ctx,
    direct_abstract_declarator: DirectAbstractDeclarator,
) -> DirectAbstractDeclaratorOpt {
    Some(Box::new(direct_abstract_declarator))
}
pub fn direct_abstract_declarator_opt_empty(_ctx: &Ctx) -> DirectAbstractDeclaratorOpt {
    None
}
pub type ParameterTypeListOpt = Option<ParameterTypeList>;
pub fn parameter_type_list_opt_parameter_type_list(
    _ctx: &Ctx,
    parameter_type_list: ParameterTypeList,
) -> ParameterTypeListOpt {
    Some(parameter_type_list)
}
pub fn parameter_type_list_opt_empty(_ctx: &Ctx) -> ParameterTypeListOpt {
    None
}
#[derive(Debug, Clone)]
pub enum ParameterTypeList {
    ParameterList1(ParameterList),
    ParameterList2(ParameterList),
}
pub fn parameter_type_list_parameter_list1(
    _ctx: &Ctx,
    parameter_list: ParameterList,
) -> ParameterTypeList {
    ParameterTypeList::ParameterList1(parameter_list)
}
pub fn parameter_type_list_parameter_list2(
    _ctx: &Ctx,
    parameter_list: ParameterList,
) -> ParameterTypeList {
    ParameterTypeList::ParameterList2(parameter_list)
}
#[derive(Debug, Clone)]
pub struct ParameterListC2 {
    pub parameter_list: Box<ParameterList>,
    pub parameter_declaration: ParameterDeclaration,
}
#[derive(Debug, Clone)]
pub enum ParameterList {
    ParameterDeclaration(ParameterDeclaration),
    C2(ParameterListC2),
}
pub fn parameter_list_parameter_declaration(
    _ctx: &Ctx,
    parameter_declaration: ParameterDeclaration,
) -> ParameterList {
    ParameterList::ParameterDeclaration(parameter_declaration)
}
pub fn parameter_list_c2(
    _ctx: &Ctx,
    parameter_list: ParameterList,
    parameter_declaration: ParameterDeclaration,
) -> ParameterList {
    ParameterList::C2(ParameterListC2 {
        parameter_list: Box::new(parameter_list),
        parameter_declaration,
    })
}
#[derive(Debug, Clone)]
pub struct ParameterDeclarationC1 {
    pub declaration_specifiers: Box<DeclarationSpecifiers>,
    pub declarator: Box<Declarator>,
}
#[derive(Debug, Clone)]
pub struct ParameterDeclarationC2 {
    pub declaration_specifiers: Box<DeclarationSpecifiers>,
    pub abstract_declarator_opt: Box<AbstractDeclaratorOpt>,
}
#[derive(Debug, Clone)]
pub enum ParameterDeclaration {
    C1(ParameterDeclarationC1),
    C2(ParameterDeclarationC2),
}
pub fn parameter_declaration_c1(
    _ctx: &Ctx,
    declaration_specifiers: DeclarationSpecifiers,
    declarator: Declarator,
) -> ParameterDeclaration {
    ParameterDeclaration::C1(ParameterDeclarationC1 {
        declaration_specifiers: Box::new(declaration_specifiers),
        declarator: Box::new(declarator),
    })
}
pub fn parameter_declaration_c2(
    _ctx: &Ctx,
    declaration_specifiers: DeclarationSpecifiers,
    abstract_declarator_opt: AbstractDeclaratorOpt,
) -> ParameterDeclaration {
    ParameterDeclaration::C2(ParameterDeclarationC2 {
        declaration_specifiers: Box::new(declaration_specifiers),
        abstract_declarator_opt: Box::new(abstract_declarator_opt),
    })
}
#[derive(Debug, Clone)]
pub struct InitializerListC1 {
    pub designation_opt: DesignationOpt,
    pub initializer: Initializer,
}
#[derive(Debug, Clone)]
pub struct InitializerListC2 {
    pub initializer_list: Box<InitializerList>,
    pub designation_opt: DesignationOpt,
    pub initializer: Initializer,
}
#[derive(Debug, Clone)]
pub enum InitializerList {
    C1(InitializerListC1),
    C2(InitializerListC2),
}
pub fn initializer_list_c1(
    _ctx: &Ctx,
    designation_opt: DesignationOpt,
    initializer: Initializer,
) -> InitializerList {
    InitializerList::C1(InitializerListC1 {
        designation_opt,
        initializer,
    })
}
pub fn initializer_list_c2(
    _ctx: &Ctx,
    initializer_list: InitializerList,
    designation_opt: DesignationOpt,
    initializer: Initializer,
) -> InitializerList {
    InitializerList::C2(InitializerListC2 {
        initializer_list: Box::new(initializer_list),
        designation_opt,
        initializer,
    })
}
pub type DesignationOpt = Option<Designation>;
pub fn designation_opt_designation(
    _ctx: &Ctx,
    designation: Designation,
) -> DesignationOpt {
    Some(designation)
}
pub fn designation_opt_empty(_ctx: &Ctx) -> DesignationOpt {
    None
}
pub type Designation = DesignatorList;
pub fn designation_designator_list(
    _ctx: &Ctx,
    designator_list: DesignatorList,
) -> Designation {
    designator_list
}
#[derive(Debug, Clone)]
pub struct DesignatorListC2 {
    pub designator_list: Box<DesignatorList>,
    pub designator: Designator,
}
#[derive(Debug, Clone)]
pub enum DesignatorList {
    Designator(Designator),
    C2(DesignatorListC2),
}
pub fn designator_list_designator(_ctx: &Ctx, designator: Designator) -> DesignatorList {
    DesignatorList::Designator(designator)
}
pub fn designator_list_c2(
    _ctx: &Ctx,
    designator_list: DesignatorList,
    designator: Designator,
) -> DesignatorList {
    DesignatorList::C2(DesignatorListC2 {
        designator_list: Box::new(designator_list),
        designator,
    })
}
#[derive(Debug, Clone)]
pub enum Designator {
    ConstantExpression(ConstantExpression),
    Identifier(Identifier),
}
pub fn designator_constant_expression(
    _ctx: &Ctx,
    constant_expression: ConstantExpression,
) -> Designator {
    Designator::ConstantExpression(constant_expression)
}
pub fn designator_identifier(_ctx: &Ctx, identifier: Identifier) -> Designator {
    Designator::Identifier(identifier)
}
pub type ConstantExpression = ConditionalExpression;
pub fn constant_expression_conditional_expression(
    _ctx: &Ctx,
    conditional_expression: ConditionalExpression,
) -> ConstantExpression {
    conditional_expression
}
#[derive(Debug, Clone)]
pub struct InitializerC2 {
    pub initializer_list: Box<InitializerList>,
    pub comma_opt: CommaOpt,
}
#[derive(Debug, Clone)]
pub enum Initializer {
    AssignmentExpression(Box<AssignmentExpression>),
    C2(InitializerC2),
}
pub fn initializer_assignment_expression(
    _ctx: &Ctx,
    assignment_expression: AssignmentExpression,
) -> Initializer {
    Initializer::AssignmentExpression(Box::new(assignment_expression))
}
pub fn initializer_c2(
    _ctx: &Ctx,
    initializer_list: InitializerList,
    comma_opt: CommaOpt,
) -> Initializer {
    Initializer::C2(InitializerC2 {
        initializer_list: Box::new(initializer_list),
        comma_opt,
    })
}
pub type CommaOpt = Option<CommaOptNoO>;
#[derive(Debug, Clone)]
pub enum CommaOptNoO {
    Comma,
}
pub fn comma_opt_comma(_ctx: &Ctx) -> CommaOpt {
    Some(CommaOptNoO::Comma)
}
pub fn comma_opt_empty(_ctx: &Ctx) -> CommaOpt {
    None
}
#[derive(Debug, Clone)]
pub struct IdentifierListC2 {
    pub identifier_list: Box<IdentifierList>,
    pub identifier: Identifier,
}
#[derive(Debug, Clone)]
pub enum IdentifierList {
    Identifier(Identifier),
    C2(IdentifierListC2),
}
pub fn identifier_list_identifier(_ctx: &Ctx, identifier: Identifier) -> IdentifierList {
    IdentifierList::Identifier(identifier)
}
pub fn identifier_list_c2(
    _ctx: &Ctx,
    identifier_list: IdentifierList,
    identifier: Identifier,
) -> IdentifierList {
    IdentifierList::C2(IdentifierListC2 {
        identifier_list: Box::new(identifier_list),
        identifier,
    })
}
#[derive(Debug, Clone)]
pub struct EnumSpecifierC1 {
    pub identifier_opt: IdentifierOpt,
    pub enumerator_list: EnumeratorList,
    pub comma_opt: CommaOpt,
}
#[derive(Debug, Clone)]
pub enum EnumSpecifier {
    C1(EnumSpecifierC1),
    Identifier(Identifier),
}
pub fn enum_specifier_c1(
    _ctx: &Ctx,
    identifier_opt: IdentifierOpt,
    enumerator_list: EnumeratorList,
    comma_opt: CommaOpt,
) -> EnumSpecifier {
    EnumSpecifier::C1(EnumSpecifierC1 {
        identifier_opt,
        enumerator_list,
        comma_opt,
    })
}
pub fn enum_specifier_identifier(_ctx: &Ctx, identifier: Identifier) -> EnumSpecifier {
    EnumSpecifier::Identifier(identifier)
}
#[derive(Debug, Clone)]
pub struct EnumeratorListC2 {
    pub enumerator_list: Box<EnumeratorList>,
    pub enumerator: Enumerator,
}
#[derive(Debug, Clone)]
pub enum EnumeratorList {
    Enumerator(Enumerator),
    C2(EnumeratorListC2),
}
pub fn enumerator_list_enumerator(_ctx: &Ctx, enumerator: Enumerator) -> EnumeratorList {
    EnumeratorList::Enumerator(enumerator)
}
pub fn enumerator_list_c2(
    _ctx: &Ctx,
    enumerator_list: EnumeratorList,
    enumerator: Enumerator,
) -> EnumeratorList {
    EnumeratorList::C2(EnumeratorListC2 {
        enumerator_list: Box::new(enumerator_list),
        enumerator,
    })
}
#[derive(Debug, Clone)]
pub struct EnumeratorC2 {
    pub enumeration_constant: EnumerationConstant,
    pub constant_expression: ConstantExpression,
}
#[derive(Debug, Clone)]
pub enum Enumerator {
    EnumerationConstant(EnumerationConstant),
    C2(EnumeratorC2),
}
pub fn enumerator_enumeration_constant(
    _ctx: &Ctx,
    enumeration_constant: EnumerationConstant,
) -> Enumerator {
    Enumerator::EnumerationConstant(enumeration_constant)
}
pub fn enumerator_c2(
    _ctx: &Ctx,
    enumeration_constant: EnumerationConstant,
    constant_expression: ConstantExpression,
) -> Enumerator {
    Enumerator::C2(EnumeratorC2 {
        enumeration_constant,
        constant_expression,
    })
}
pub type EnumerationConstant = Identifier;
pub fn enumeration_constant_identifier(
    _ctx: &Ctx,
    identifier: Identifier,
) -> EnumerationConstant {
    identifier
}
pub type TypedefName = Identifier;
pub fn typedef_name_identifier(_ctx: &Ctx, identifier: Identifier) -> TypedefName {
    identifier
}
#[derive(Debug, Clone)]
pub enum FunctionSpecifier {
    Inline,
}
pub fn function_specifier_inline(_ctx: &Ctx) -> FunctionSpecifier {
    FunctionSpecifier::Inline
}
#[derive(Debug, Clone)]
pub struct DeclarationListC2 {
    pub declaration_list: Box<DeclarationList>,
    pub declaration: Declaration,
}
#[derive(Debug, Clone)]
pub enum DeclarationList {
    Declaration(Declaration),
    C2(DeclarationListC2),
}
pub fn declaration_list_declaration(
    _ctx: &Ctx,
    declaration: Declaration,
) -> DeclarationList {
    DeclarationList::Declaration(declaration)
}
pub fn declaration_list_c2(
    _ctx: &Ctx,
    declaration_list: DeclarationList,
    declaration: Declaration,
) -> DeclarationList {
    DeclarationList::C2(DeclarationListC2 {
        declaration_list: Box::new(declaration_list),
        declaration,
    })
}
#[derive(Debug, Clone)]
pub struct Declaration {
    pub declaration_specifiers: DeclarationSpecifiers,
    pub init_declarator_list_opt: InitDeclaratorListOpt,
}
pub fn declaration_c1(
    _ctx: &Ctx,
    declaration_specifiers: DeclarationSpecifiers,
    init_declarator_list_opt: InitDeclaratorListOpt,
) -> Declaration {
    Declaration {
        declaration_specifiers,
        init_declarator_list_opt,
    }
}
pub type InitDeclaratorListOpt = Option<InitDeclaratorList>;
pub fn init_declarator_list_opt_init_declarator_list(
    _ctx: &Ctx,
    init_declarator_list: InitDeclaratorList,
) -> InitDeclaratorListOpt {
    Some(init_declarator_list)
}
pub fn init_declarator_list_opt_empty(_ctx: &Ctx) -> InitDeclaratorListOpt {
    None
}
#[derive(Debug, Clone)]
pub struct InitDeclaratorListC2 {
    pub init_declarator_list: Box<InitDeclaratorList>,
    pub init_declarator: InitDeclarator,
}
#[derive(Debug, Clone)]
pub enum InitDeclaratorList {
    InitDeclarator(InitDeclarator),
    C2(InitDeclaratorListC2),
}
pub fn init_declarator_list_init_declarator(
    _ctx: &Ctx,
    init_declarator: InitDeclarator,
) -> InitDeclaratorList {
    InitDeclaratorList::InitDeclarator(init_declarator)
}
pub fn init_declarator_list_c2(
    _ctx: &Ctx,
    init_declarator_list: InitDeclaratorList,
    init_declarator: InitDeclarator,
) -> InitDeclaratorList {
    InitDeclaratorList::C2(InitDeclaratorListC2 {
        init_declarator_list: Box::new(init_declarator_list),
        init_declarator,
    })
}
#[derive(Debug, Clone)]
pub struct InitDeclaratorC2 {
    pub declarator: Declarator,
    pub initializer: Initializer,
}
#[derive(Debug, Clone)]
pub enum InitDeclarator {
    Declarator(Declarator),
    C2(InitDeclaratorC2),
}
pub fn init_declarator_declarator(_ctx: &Ctx, declarator: Declarator) -> InitDeclarator {
    InitDeclarator::Declarator(declarator)
}
pub fn init_declarator_c2(
    _ctx: &Ctx,
    declarator: Declarator,
    initializer: Initializer,
) -> InitDeclarator {
    InitDeclarator::C2(InitDeclaratorC2 {
        declarator,
        initializer,
    })
}
pub type CompoundStatement = BlockItemListOpt;
pub fn compound_statement_block_item_list_opt(
    _ctx: &Ctx,
    block_item_list_opt: BlockItemListOpt,
) -> CompoundStatement {
    block_item_list_opt
}
pub type BlockItemListOpt = Option<BlockItemList>;
pub fn block_item_list_opt_block_item_list(
    _ctx: &Ctx,
    block_item_list: BlockItemList,
) -> BlockItemListOpt {
    Some(block_item_list)
}
pub fn block_item_list_opt_empty(_ctx: &Ctx) -> BlockItemListOpt {
    None
}
#[derive(Debug, Clone)]
pub struct BlockItemListC2 {
    pub block_item_list: Box<BlockItemList>,
    pub block_item: BlockItem,
}
#[derive(Debug, Clone)]
pub enum BlockItemList {
    BlockItem(BlockItem),
    C2(BlockItemListC2),
}
pub fn block_item_list_block_item(_ctx: &Ctx, block_item: BlockItem) -> BlockItemList {
    BlockItemList::BlockItem(block_item)
}
pub fn block_item_list_c2(
    _ctx: &Ctx,
    block_item_list: BlockItemList,
    block_item: BlockItem,
) -> BlockItemList {
    BlockItemList::C2(BlockItemListC2 {
        block_item_list: Box::new(block_item_list),
        block_item,
    })
}
#[derive(Debug, Clone)]
pub enum BlockItem {
    Declaration(Declaration),
    Statement(Statement),
}
pub fn block_item_declaration(_ctx: &Ctx, declaration: Declaration) -> BlockItem {
    BlockItem::Declaration(declaration)
}
pub fn block_item_statement(_ctx: &Ctx, statement: Statement) -> BlockItem {
    BlockItem::Statement(statement)
}
#[derive(Debug, Clone)]
pub enum Statement {
    LabeledStatement(LabeledStatement),
    CompoundStatement(Box<CompoundStatement>),
    ExpressionStatement(ExpressionStatement),
    SelectionStatement(SelectionStatement),
    IterationStatement(IterationStatement),
    JumpStatement(JumpStatement),
}
pub fn statement_labeled_statement(
    _ctx: &Ctx,
    labeled_statement: LabeledStatement,
) -> Statement {
    Statement::LabeledStatement(labeled_statement)
}
pub fn statement_compound_statement(
    _ctx: &Ctx,
    compound_statement: CompoundStatement,
) -> Statement {
    Statement::CompoundStatement(Box::new(compound_statement))
}
pub fn statement_expression_statement(
    _ctx: &Ctx,
    expression_statement: ExpressionStatement,
) -> Statement {
    Statement::ExpressionStatement(expression_statement)
}
pub fn statement_selection_statement(
    _ctx: &Ctx,
    selection_statement: SelectionStatement,
) -> Statement {
    Statement::SelectionStatement(selection_statement)
}
pub fn statement_iteration_statement(
    _ctx: &Ctx,
    iteration_statement: IterationStatement,
) -> Statement {
    Statement::IterationStatement(iteration_statement)
}
pub fn statement_jump_statement(_ctx: &Ctx, jump_statement: JumpStatement) -> Statement {
    Statement::JumpStatement(jump_statement)
}
#[derive(Debug, Clone)]
pub struct LabeledStatementC1 {
    pub identifier: Identifier,
    pub statement: Box<Statement>,
}
#[derive(Debug, Clone)]
pub struct LabeledStatementC2 {
    pub constant_expression: ConstantExpression,
    pub statement: Box<Statement>,
}
#[derive(Debug, Clone)]
pub enum LabeledStatement {
    C1(LabeledStatementC1),
    C2(LabeledStatementC2),
    Statement(Box<Statement>),
}
pub fn labeled_statement_c1(
    _ctx: &Ctx,
    identifier: Identifier,
    statement: Statement,
) -> LabeledStatement {
    LabeledStatement::C1(LabeledStatementC1 {
        identifier,
        statement: Box::new(statement),
    })
}
pub fn labeled_statement_c2(
    _ctx: &Ctx,
    constant_expression: ConstantExpression,
    statement: Statement,
) -> LabeledStatement {
    LabeledStatement::C2(LabeledStatementC2 {
        constant_expression,
        statement: Box::new(statement),
    })
}
pub fn labeled_statement_statement(
    _ctx: &Ctx,
    statement: Statement,
) -> LabeledStatement {
    LabeledStatement::Statement(Box::new(statement))
}
pub type ExpressionStatement = ExpressionOpt;
pub fn expression_statement_expression_opt(
    _ctx: &Ctx,
    expression_opt: ExpressionOpt,
) -> ExpressionStatement {
    expression_opt
}
pub type ExpressionOpt = Option<Expression>;
pub fn expression_opt_expression(_ctx: &Ctx, expression: Expression) -> ExpressionOpt {
    Some(expression)
}
pub fn expression_opt_empty(_ctx: &Ctx) -> ExpressionOpt {
    None
}
#[derive(Debug, Clone)]
pub struct SelectionStatementC1 {
    pub expression: Expression,
    pub statement: Box<Statement>,
}
#[derive(Debug, Clone)]
pub struct SelectionStatementC2 {
    pub expression: Expression,
    pub statement_5: Box<Statement>,
    pub statement_7: Box<Statement>,
}
#[derive(Debug, Clone)]
pub struct SelectionStatementC3 {
    pub expression: Expression,
    pub statement: Box<Statement>,
}
#[derive(Debug, Clone)]
pub enum SelectionStatement {
    C1(SelectionStatementC1),
    C2(SelectionStatementC2),
    C3(SelectionStatementC3),
}
pub fn selection_statement_c1(
    _ctx: &Ctx,
    expression: Expression,
    statement: Statement,
) -> SelectionStatement {
    SelectionStatement::C1(SelectionStatementC1 {
        expression,
        statement: Box::new(statement),
    })
}
pub fn selection_statement_c2(
    _ctx: &Ctx,
    expression: Expression,
    statement_5: Statement,
    statement_7: Statement,
) -> SelectionStatement {
    SelectionStatement::C2(SelectionStatementC2 {
        expression,
        statement_5: Box::new(statement_5),
        statement_7: Box::new(statement_7),
    })
}
pub fn selection_statement_c3(
    _ctx: &Ctx,
    expression: Expression,
    statement: Statement,
) -> SelectionStatement {
    SelectionStatement::C3(SelectionStatementC3 {
        expression,
        statement: Box::new(statement),
    })
}
#[derive(Debug, Clone)]
pub struct IterationStatementC1 {
    pub expression: Expression,
    pub statement: Box<Statement>,
}
#[derive(Debug, Clone)]
pub struct IterationStatementC2 {
    pub statement: Box<Statement>,
    pub expression: Expression,
}
#[derive(Debug, Clone)]
pub struct IterationStatementC3 {
    pub expression_opt_3: ExpressionOpt,
    pub expression_opt_5: ExpressionOpt,
    pub expression_opt_7: ExpressionOpt,
    pub statement: Box<Statement>,
}
#[derive(Debug, Clone)]
pub struct IterationStatementC4 {
    pub declaration: Declaration,
    pub expression_opt_4: ExpressionOpt,
    pub expression_opt_6: ExpressionOpt,
    pub statement: Box<Statement>,
}
#[derive(Debug, Clone)]
pub enum IterationStatement {
    C1(IterationStatementC1),
    C2(IterationStatementC2),
    C3(IterationStatementC3),
    C4(IterationStatementC4),
}
pub fn iteration_statement_c1(
    _ctx: &Ctx,
    expression: Expression,
    statement: Statement,
) -> IterationStatement {
    IterationStatement::C1(IterationStatementC1 {
        expression,
        statement: Box::new(statement),
    })
}
pub fn iteration_statement_c2(
    _ctx: &Ctx,
    statement: Statement,
    expression: Expression,
) -> IterationStatement {
    IterationStatement::C2(IterationStatementC2 {
        statement: Box::new(statement),
        expression,
    })
}
pub fn iteration_statement_c3(
    _ctx: &Ctx,
    expression_opt_3: ExpressionOpt,
    expression_opt_5: ExpressionOpt,
    expression_opt_7: ExpressionOpt,
    statement: Statement,
) -> IterationStatement {
    IterationStatement::C3(IterationStatementC3 {
        expression_opt_3,
        expression_opt_5,
        expression_opt_7,
        statement: Box::new(statement),
    })
}
pub fn iteration_statement_c4(
    _ctx: &Ctx,
    declaration: Declaration,
    expression_opt_4: ExpressionOpt,
    expression_opt_6: ExpressionOpt,
    statement: Statement,
) -> IterationStatement {
    IterationStatement::C4(IterationStatementC4 {
        declaration,
        expression_opt_4,
        expression_opt_6,
        statement: Box::new(statement),
    })
}
#[derive(Debug, Clone)]
pub enum JumpStatement {
    Identifier(Identifier),
    C2,
    C3,
    ExpressionOpt(ExpressionOpt),
}
pub fn jump_statement_identifier(_ctx: &Ctx, identifier: Identifier) -> JumpStatement {
    JumpStatement::Identifier(identifier)
}
pub fn jump_statement_c2(_ctx: &Ctx) -> JumpStatement {
    JumpStatement::C2
}
pub fn jump_statement_c3(_ctx: &Ctx) -> JumpStatement {
    JumpStatement::C3
}
pub fn jump_statement_expression_opt(
    _ctx: &Ctx,
    expression_opt: ExpressionOpt,
) -> JumpStatement {
    JumpStatement::ExpressionOpt(expression_opt)
}
