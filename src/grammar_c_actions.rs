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
pub type StringLiteral = String;
pub fn string_literal(_ctx: &Ctx, token: Token) -> StringLiteral {
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
    pub declaration_specifier1: DeclarationSpecifier1,
    pub declarator: Declarator,
    pub declaration0: Declaration0,
    pub compound_statement: CompoundStatement,
}
pub fn function_definition_c1(
    _ctx: &Ctx,
    declaration_specifier1: DeclarationSpecifier1,
    declarator: Declarator,
    declaration0: Declaration0,
    compound_statement: CompoundStatement,
) -> FunctionDefinition {
    FunctionDefinition {
        declaration_specifier1,
        declarator,
        declaration0,
        compound_statement,
    }
}
pub type DeclarationSpecifier1 = Vec<DeclarationSpecifier>;
pub fn declaration_specifier1_c1(
    _ctx: &Ctx,
    mut declaration_specifier1: DeclarationSpecifier1,
    declaration_specifier: DeclarationSpecifier,
) -> DeclarationSpecifier1 {
    declaration_specifier1.push(declaration_specifier);
    declaration_specifier1
}
pub fn declaration_specifier1_declaration_specifier(
    _ctx: &Ctx,
    declaration_specifier: DeclarationSpecifier,
) -> DeclarationSpecifier1 {
    vec![declaration_specifier]
}
pub type Declaration1 = Vec<Declaration>;
pub fn declaration1_c1(
    _ctx: &Ctx,
    mut declaration1: Declaration1,
    declaration: Declaration,
) -> Declaration1 {
    declaration1.push(declaration);
    declaration1
}
pub fn declaration1_declaration(_ctx: &Ctx, declaration: Declaration) -> Declaration1 {
    vec![declaration]
}
pub type Declaration0 = Option<Declaration1>;
pub fn declaration0_declaration1(
    _ctx: &Ctx,
    declaration1: Declaration1,
) -> Declaration0 {
    Some(declaration1)
}
pub fn declaration0_empty(_ctx: &Ctx) -> Declaration0 {
    None
}
#[derive(Debug, Clone)]
pub enum DeclarationSpecifier {
    StorageClassSpecifier(StorageClassSpecifier),
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
    FunctionSpecifier(FunctionSpecifier),
}
pub fn declaration_specifier_storage_class_specifier(
    _ctx: &Ctx,
    storage_class_specifier: StorageClassSpecifier,
) -> DeclarationSpecifier {
    DeclarationSpecifier::StorageClassSpecifier(storage_class_specifier)
}
pub fn declaration_specifier_type_specifier(
    _ctx: &Ctx,
    type_specifier: TypeSpecifier,
) -> DeclarationSpecifier {
    DeclarationSpecifier::TypeSpecifier(type_specifier)
}
pub fn declaration_specifier_type_qualifier(
    _ctx: &Ctx,
    type_qualifier: TypeQualifier,
) -> DeclarationSpecifier {
    DeclarationSpecifier::TypeQualifier(type_qualifier)
}
pub fn declaration_specifier_function_specifier(
    _ctx: &Ctx,
    function_specifier: FunctionSpecifier,
) -> DeclarationSpecifier {
    DeclarationSpecifier::FunctionSpecifier(function_specifier)
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
    pub struct_declaration1: StructDeclaration1,
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
    struct_declaration1: StructDeclaration1,
) -> StructOrUnionSpecifier {
    StructOrUnionSpecifier::C1(StructOrUnionSpecifierC1 {
        struct_or_union,
        identifier_opt,
        struct_declaration1,
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
pub type StructDeclaration1 = Vec<StructDeclaration>;
pub fn struct_declaration1_c1(
    _ctx: &Ctx,
    mut struct_declaration1: StructDeclaration1,
    struct_declaration: StructDeclaration,
) -> StructDeclaration1 {
    struct_declaration1.push(struct_declaration);
    struct_declaration1
}
pub fn struct_declaration1_struct_declaration(
    _ctx: &Ctx,
    struct_declaration: StructDeclaration,
) -> StructDeclaration1 {
    vec![struct_declaration]
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
pub struct StructDeclaration {
    pub specifier_qualifier_kind1: SpecifierQualifierKind1,
    pub struct_declarator_list: StructDeclaratorList,
}
pub fn struct_declaration_c1(
    _ctx: &Ctx,
    specifier_qualifier_kind1: SpecifierQualifierKind1,
    struct_declarator_list: StructDeclaratorList,
) -> StructDeclaration {
    StructDeclaration {
        specifier_qualifier_kind1,
        struct_declarator_list,
    }
}
pub type SpecifierQualifierKind1 = Vec<SpecifierQualifierKind>;
pub fn specifier_qualifier_kind1_c1(
    _ctx: &Ctx,
    mut specifier_qualifier_kind1: SpecifierQualifierKind1,
    specifier_qualifier_kind: SpecifierQualifierKind,
) -> SpecifierQualifierKind1 {
    specifier_qualifier_kind1.push(specifier_qualifier_kind);
    specifier_qualifier_kind1
}
pub fn specifier_qualifier_kind1_specifier_qualifier_kind(
    _ctx: &Ctx,
    specifier_qualifier_kind: SpecifierQualifierKind,
) -> SpecifierQualifierKind1 {
    vec![specifier_qualifier_kind]
}
#[derive(Debug, Clone)]
pub enum SpecifierQualifierKind {
    TypeSpecifier(Box<TypeSpecifier>),
    TypeQualifier(TypeQualifier),
}
pub fn specifier_qualifier_kind_type_specifier(
    _ctx: &Ctx,
    type_specifier: TypeSpecifier,
) -> SpecifierQualifierKind {
    SpecifierQualifierKind::TypeSpecifier(Box::new(type_specifier))
}
pub fn specifier_qualifier_kind_type_qualifier(
    _ctx: &Ctx,
    type_qualifier: TypeQualifier,
) -> SpecifierQualifierKind {
    SpecifierQualifierKind::TypeQualifier(type_qualifier)
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
pub struct StructDeclaratorListC1 {
    pub struct_declarator_list: Box<StructDeclaratorList>,
    pub struct_declarator: StructDeclarator,
}
#[derive(Debug, Clone)]
pub enum StructDeclaratorList {
    C1(StructDeclaratorListC1),
    StructDeclarator(StructDeclarator),
}
pub fn struct_declarator_list_c1(
    _ctx: &Ctx,
    struct_declarator_list: StructDeclaratorList,
    struct_declarator: StructDeclarator,
) -> StructDeclaratorList {
    StructDeclaratorList::C1(StructDeclaratorListC1 {
        struct_declarator_list: Box::new(struct_declarator_list),
        struct_declarator,
    })
}
pub fn struct_declarator_list_struct_declarator(
    _ctx: &Ctx,
    struct_declarator: StructDeclarator,
) -> StructDeclaratorList {
    StructDeclaratorList::StructDeclarator(struct_declarator)
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
    pub type_qualifier0: TypeQualifier0,
    pub pointer: Box<Pointer>,
}
#[derive(Debug, Clone)]
pub enum Pointer {
    TypeQualifier0(TypeQualifier0),
    C2(PointerC2),
}
pub fn pointer_type_qualifier0(_ctx: &Ctx, type_qualifier0: TypeQualifier0) -> Pointer {
    Pointer::TypeQualifier0(type_qualifier0)
}
pub fn pointer_c2(
    _ctx: &Ctx,
    type_qualifier0: TypeQualifier0,
    pointer: Pointer,
) -> Pointer {
    Pointer::C2(PointerC2 {
        type_qualifier0,
        pointer: Box::new(pointer),
    })
}
pub type TypeQualifier1 = Vec<TypeQualifier>;
pub fn type_qualifier1_c1(
    _ctx: &Ctx,
    mut type_qualifier1: TypeQualifier1,
    type_qualifier: TypeQualifier,
) -> TypeQualifier1 {
    type_qualifier1.push(type_qualifier);
    type_qualifier1
}
pub fn type_qualifier1_type_qualifier(
    _ctx: &Ctx,
    type_qualifier: TypeQualifier,
) -> TypeQualifier1 {
    vec![type_qualifier]
}
pub type TypeQualifier0 = Option<TypeQualifier1>;
pub fn type_qualifier0_type_qualifier1(
    _ctx: &Ctx,
    type_qualifier1: TypeQualifier1,
) -> TypeQualifier0 {
    Some(type_qualifier1)
}
pub fn type_qualifier0_empty(_ctx: &Ctx) -> TypeQualifier0 {
    None
}
#[derive(Debug, Clone)]
pub struct DirectDeclaratorC3 {
    pub direct_declarator: Box<DirectDeclarator>,
    pub type_qualifier0: TypeQualifier0,
    pub assignment_expression_opt: AssignmentExpressionOpt,
}
#[derive(Debug, Clone)]
pub struct DirectDeclaratorC4 {
    pub direct_declarator: Box<DirectDeclarator>,
    pub type_qualifier0: TypeQualifier0,
    pub assignment_expression: AssignmentExpression,
}
#[derive(Debug, Clone)]
pub struct DirectDeclaratorC5 {
    pub direct_declarator: Box<DirectDeclarator>,
    pub type_qualifier1: TypeQualifier1,
    pub assignment_expression: AssignmentExpression,
}
#[derive(Debug, Clone)]
pub struct DirectDeclaratorC6 {
    pub direct_declarator: Box<DirectDeclarator>,
    pub type_qualifier0: TypeQualifier0,
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
    type_qualifier0: TypeQualifier0,
    assignment_expression_opt: AssignmentExpressionOpt,
) -> DirectDeclarator {
    DirectDeclarator::C3(DirectDeclaratorC3 {
        direct_declarator: Box::new(direct_declarator),
        type_qualifier0,
        assignment_expression_opt,
    })
}
pub fn direct_declarator_c4(
    _ctx: &Ctx,
    direct_declarator: DirectDeclarator,
    type_qualifier0: TypeQualifier0,
    assignment_expression: AssignmentExpression,
) -> DirectDeclarator {
    DirectDeclarator::C4(DirectDeclaratorC4 {
        direct_declarator: Box::new(direct_declarator),
        type_qualifier0,
        assignment_expression,
    })
}
pub fn direct_declarator_c5(
    _ctx: &Ctx,
    direct_declarator: DirectDeclarator,
    type_qualifier1: TypeQualifier1,
    assignment_expression: AssignmentExpression,
) -> DirectDeclarator {
    DirectDeclarator::C5(DirectDeclaratorC5 {
        direct_declarator: Box::new(direct_declarator),
        type_qualifier1,
        assignment_expression,
    })
}
pub fn direct_declarator_c6(
    _ctx: &Ctx,
    direct_declarator: DirectDeclarator,
    type_qualifier0: TypeQualifier0,
) -> DirectDeclarator {
    DirectDeclarator::C6(DirectDeclaratorC6 {
        direct_declarator: Box::new(direct_declarator),
        type_qualifier0,
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
pub struct CommaExpression {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}
#[derive(Debug, Clone)]
pub enum Expression {
    CommaExpression(CommaExpression),
    AssignmentExpression(Box<AssignmentExpression>),
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
#[derive(Debug, Clone)]
pub enum AssignmentOperators {
    Equal,
    PlusEqual,
    DashEqual,
    StarEqual,
    SlashEqual,
    PercentEqual,
    DoubleLeftEqual,
    DoubleRightEqual,
    AmpersandEqual,
    CaretEqual,
    VerticalBarEqual,
}
pub fn assignment_operators_equal(_ctx: &Ctx) -> AssignmentOperators {
    AssignmentOperators::Equal
}
pub fn assignment_operators_plus_equal(_ctx: &Ctx) -> AssignmentOperators {
    AssignmentOperators::PlusEqual
}
pub fn assignment_operators_dash_equal(_ctx: &Ctx) -> AssignmentOperators {
    AssignmentOperators::DashEqual
}
pub fn assignment_operators_star_equal(_ctx: &Ctx) -> AssignmentOperators {
    AssignmentOperators::StarEqual
}
pub fn assignment_operators_slash_equal(_ctx: &Ctx) -> AssignmentOperators {
    AssignmentOperators::SlashEqual
}
pub fn assignment_operators_percent_equal(_ctx: &Ctx) -> AssignmentOperators {
    AssignmentOperators::PercentEqual
}
pub fn assignment_operators_double_left_equal(_ctx: &Ctx) -> AssignmentOperators {
    AssignmentOperators::DoubleLeftEqual
}
pub fn assignment_operators_double_right_equal(_ctx: &Ctx) -> AssignmentOperators {
    AssignmentOperators::DoubleRightEqual
}
pub fn assignment_operators_ampersand_equal(_ctx: &Ctx) -> AssignmentOperators {
    AssignmentOperators::AmpersandEqual
}
pub fn assignment_operators_caret_equal(_ctx: &Ctx) -> AssignmentOperators {
    AssignmentOperators::CaretEqual
}
pub fn assignment_operators_vertical_bar_equal(_ctx: &Ctx) -> AssignmentOperators {
    AssignmentOperators::VerticalBarEqual
}
#[derive(Debug, Clone)]
pub struct AssignmentExpressionC2 {
    pub left: UnaryExpression,
    pub assignment_operators: AssignmentOperators,
    pub right: Expression,
}
#[derive(Debug, Clone)]
pub enum AssignmentExpression {
    ConditionalExpression(ConditionalExpression),
    C2(AssignmentExpressionC2),
}
pub fn assignment_expression_conditional_expression(
    _ctx: &Ctx,
    conditional_expression: ConditionalExpression,
) -> AssignmentExpression {
    AssignmentExpression::ConditionalExpression(conditional_expression)
}
pub fn assignment_expression_c2(
    _ctx: &Ctx,
    left: UnaryExpression,
    assignment_operators: AssignmentOperators,
    right: Expression,
) -> AssignmentExpression {
    AssignmentExpression::C2(AssignmentExpressionC2 {
        left,
        assignment_operators,
        right,
    })
}
#[derive(Debug, Clone)]
pub struct ConditionalExpressionC2 {
    pub condition: Expression,
    pub branch_true: Expression,
    pub branch_false: Expression,
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
    condition: Expression,
    branch_true: Expression,
    branch_false: Expression,
) -> ConditionalExpression {
    ConditionalExpression::C2(ConditionalExpressionC2 {
        condition,
        branch_true,
        branch_false,
    })
}
#[derive(Debug, Clone)]
pub struct LogicalOrExpression {
    pub left: Expression,
    pub right: Expression,
}
#[derive(Debug, Clone)]
pub struct LogicalAndExpression {
    pub left: Expression,
    pub right: Expression,
}
#[derive(Debug, Clone)]
pub struct BitwiseOrExpression {
    pub left: Expression,
    pub right: Expression,
}
#[derive(Debug, Clone)]
pub struct BitwiseExclusiveOrExpression {
    pub left: Expression,
    pub right: Expression,
}
#[derive(Debug, Clone)]
pub struct BitwiseAndExpression {
    pub left: Expression,
    pub right: Expression,
}
#[derive(Debug, Clone)]
pub struct LogicalEqualExpression {
    pub left: Expression,
    pub right: Expression,
}
#[derive(Debug, Clone)]
pub struct LogicalNotEqualExpression {
    pub left: Expression,
    pub right: Expression,
}
#[derive(Debug, Clone)]
pub struct LessThanExpression {
    pub left: Expression,
    pub right: Expression,
}
#[derive(Debug, Clone)]
pub struct LessThanOrEqualExpression {
    pub left: Expression,
    pub right: Expression,
}
#[derive(Debug, Clone)]
pub struct GreaterThanExpression {
    pub left: Expression,
    pub right: Expression,
}
#[derive(Debug, Clone)]
pub struct GreaterThanOrEqualExpression {
    pub left: Expression,
    pub right: Expression,
}
#[derive(Debug, Clone)]
pub struct BitwiseLeftShiftExpression {
    pub left: Expression,
    pub right: Expression,
}
#[derive(Debug, Clone)]
pub struct BitwiseRightShiftExpression {
    pub left: Expression,
    pub right: Expression,
}
#[derive(Debug, Clone)]
pub struct AdditionExpression {
    pub left: Expression,
    pub right: Expression,
}
#[derive(Debug, Clone)]
pub struct SubtractionExpression {
    pub left: Expression,
    pub right: Expression,
}
#[derive(Debug, Clone)]
pub struct MultiplyExpression {
    pub left: Expression,
    pub right: Expression,
}
#[derive(Debug, Clone)]
pub struct DivisionExpression {
    pub left: Expression,
    pub right: Expression,
}
#[derive(Debug, Clone)]
pub struct ModuloExpression {
    pub left: Expression,
    pub right: Expression,
}
#[derive(Debug, Clone)]
pub enum ArithmeticExpression {
    UnaryExpression(UnaryExpression),
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
pub fn arithmetic_expression_unary_expression(
    _ctx: &Ctx,
    unary_expression: UnaryExpression,
) -> ArithmeticExpression {
    ArithmeticExpression::UnaryExpression(unary_expression)
}
pub fn arithmetic_expression_logical_or_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::LogicalOrExpression(LogicalOrExpression { left, right })
}
pub fn arithmetic_expression_logical_and_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::LogicalAndExpression(LogicalAndExpression {
        left,
        right,
    })
}
pub fn arithmetic_expression_bitwise_or_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::BitwiseOrExpression(BitwiseOrExpression { left, right })
}
pub fn arithmetic_expression_bitwise_exclusive_or_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::BitwiseExclusiveOrExpression(BitwiseExclusiveOrExpression {
        left,
        right,
    })
}
pub fn arithmetic_expression_bitwise_and_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::BitwiseAndExpression(BitwiseAndExpression {
        left,
        right,
    })
}
pub fn arithmetic_expression_logical_equal_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::LogicalEqualExpression(LogicalEqualExpression {
        left,
        right,
    })
}
pub fn arithmetic_expression_logical_not_equal_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::LogicalNotEqualExpression(LogicalNotEqualExpression {
        left,
        right,
    })
}
pub fn arithmetic_expression_less_than_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::LessThanExpression(LessThanExpression { left, right })
}
pub fn arithmetic_expression_less_than_or_equal_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::LessThanOrEqualExpression(LessThanOrEqualExpression {
        left,
        right,
    })
}
pub fn arithmetic_expression_greater_than_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::GreaterThanExpression(GreaterThanExpression {
        left,
        right,
    })
}
pub fn arithmetic_expression_greater_than_or_equal_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::GreaterThanOrEqualExpression(GreaterThanOrEqualExpression {
        left,
        right,
    })
}
pub fn arithmetic_expression_bitwise_left_shift_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::BitwiseLeftShiftExpression(BitwiseLeftShiftExpression {
        left,
        right,
    })
}
pub fn arithmetic_expression_bitwise_right_shift_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::BitwiseRightShiftExpression(BitwiseRightShiftExpression {
        left,
        right,
    })
}
pub fn arithmetic_expression_addition_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::AdditionExpression(AdditionExpression { left, right })
}
pub fn arithmetic_expression_subtraction_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::SubtractionExpression(SubtractionExpression {
        left,
        right,
    })
}
pub fn arithmetic_expression_multiply_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::MultiplyExpression(MultiplyExpression { left, right })
}
pub fn arithmetic_expression_division_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::DivisionExpression(DivisionExpression { left, right })
}
pub fn arithmetic_expression_modulo_expression(
    _ctx: &Ctx,
    left: Expression,
    right: Expression,
) -> ArithmeticExpression {
    ArithmeticExpression::ModuloExpression(ModuloExpression { left, right })
}
pub type TypeCast = TypeName;
pub fn type_cast_type_name(_ctx: &Ctx, type_name: TypeName) -> TypeCast {
    type_name
}
#[derive(Debug, Clone)]
pub struct LogicalNotExpression {
    pub type_cast_opt: TypeCastOpt,
    pub unary_expression: Box<UnaryExpression>,
}
#[derive(Debug, Clone)]
pub struct BitwiseNotExpression {
    pub type_cast_opt: TypeCastOpt,
    pub unary_expression: Box<UnaryExpression>,
}
#[derive(Debug, Clone)]
pub struct PlusExpression {
    pub type_cast_opt: TypeCastOpt,
    pub unary_expression: Box<UnaryExpression>,
}
#[derive(Debug, Clone)]
pub struct MinusExpression {
    pub type_cast_opt: TypeCastOpt,
    pub unary_expression: Box<UnaryExpression>,
}
#[derive(Debug, Clone)]
pub struct DereferenceExpression {
    pub type_cast_opt: TypeCastOpt,
    pub unary_expression: Box<UnaryExpression>,
}
#[derive(Debug, Clone)]
pub struct ReferenceExpression {
    pub type_cast_opt: TypeCastOpt,
    pub unary_expression: Box<UnaryExpression>,
}
#[derive(Debug, Clone)]
pub struct ArrayAccessExpression {
    pub assumed_array: Box<UnaryExpression>,
    pub index: Expression,
}
#[derive(Debug, Clone)]
pub struct FunctionCallExpression {
    pub assumed_callable: Box<UnaryExpression>,
    pub arguments: ArgumentExpressionListOpt,
}
#[derive(Debug, Clone)]
pub struct AccessExpression {
    pub assumed_variable: Box<UnaryExpression>,
    pub identifier: Identifier,
}
#[derive(Debug, Clone)]
pub struct PointerAccessExpression {
    pub assumed_pointer: Box<UnaryExpression>,
    pub identifier: Identifier,
}
#[derive(Debug, Clone)]
pub struct PostIncrementExpression {
    pub assumed_increment: Box<UnaryExpression>,
}
#[derive(Debug, Clone)]
pub struct PostDecrementExpression {
    pub assumed_decrement: Box<UnaryExpression>,
}
#[derive(Debug, Clone)]
pub struct CompoundLiteralExpression {
    pub type_cast: TypeCast,
    pub initializer_list: InitializerList,
    pub comma_opt: CommaOpt,
}
#[derive(Debug, Clone)]
pub enum UnaryExpression {
    PrimaryExpression(PrimaryExpression),
    PreIncrementExpression(Box<UnaryExpression>),
    PreDecrementExpression(Box<UnaryExpression>),
    LogicalNotExpression(LogicalNotExpression),
    BitwiseNotExpression(BitwiseNotExpression),
    PlusExpression(PlusExpression),
    MinusExpression(MinusExpression),
    DereferenceExpression(DereferenceExpression),
    ReferenceExpression(ReferenceExpression),
    SizeOfExpressionExpression(Expression),
    SizeOfTypenameExpression(TypeName),
    ArrayAccessExpression(ArrayAccessExpression),
    FunctionCallExpression(FunctionCallExpression),
    AccessExpression(AccessExpression),
    PointerAccessExpression(PointerAccessExpression),
    PostIncrementExpression(PostIncrementExpression),
    PostDecrementExpression(PostDecrementExpression),
    CompoundLiteralExpression(CompoundLiteralExpression),
}
pub fn unary_expression_primary_expression(
    _ctx: &Ctx,
    primary_expression: PrimaryExpression,
) -> UnaryExpression {
    UnaryExpression::PrimaryExpression(primary_expression)
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
    type_cast_opt: TypeCastOpt,
    unary_expression: UnaryExpression,
) -> UnaryExpression {
    UnaryExpression::LogicalNotExpression(LogicalNotExpression {
        type_cast_opt,
        unary_expression: Box::new(unary_expression),
    })
}
pub fn unary_expression_bitwise_not_expression(
    _ctx: &Ctx,
    type_cast_opt: TypeCastOpt,
    unary_expression: UnaryExpression,
) -> UnaryExpression {
    UnaryExpression::BitwiseNotExpression(BitwiseNotExpression {
        type_cast_opt,
        unary_expression: Box::new(unary_expression),
    })
}
pub fn unary_expression_plus_expression(
    _ctx: &Ctx,
    type_cast_opt: TypeCastOpt,
    unary_expression: UnaryExpression,
) -> UnaryExpression {
    UnaryExpression::PlusExpression(PlusExpression {
        type_cast_opt,
        unary_expression: Box::new(unary_expression),
    })
}
pub fn unary_expression_minus_expression(
    _ctx: &Ctx,
    type_cast_opt: TypeCastOpt,
    unary_expression: UnaryExpression,
) -> UnaryExpression {
    UnaryExpression::MinusExpression(MinusExpression {
        type_cast_opt,
        unary_expression: Box::new(unary_expression),
    })
}
pub fn unary_expression_dereference_expression(
    _ctx: &Ctx,
    type_cast_opt: TypeCastOpt,
    unary_expression: UnaryExpression,
) -> UnaryExpression {
    UnaryExpression::DereferenceExpression(DereferenceExpression {
        type_cast_opt,
        unary_expression: Box::new(unary_expression),
    })
}
pub fn unary_expression_reference_expression(
    _ctx: &Ctx,
    type_cast_opt: TypeCastOpt,
    unary_expression: UnaryExpression,
) -> UnaryExpression {
    UnaryExpression::ReferenceExpression(ReferenceExpression {
        type_cast_opt,
        unary_expression: Box::new(unary_expression),
    })
}
pub fn unary_expression_size_of_expression_expression(
    _ctx: &Ctx,
    expression: Expression,
) -> UnaryExpression {
    UnaryExpression::SizeOfExpressionExpression(expression)
}
pub fn unary_expression_size_of_typename_expression(
    _ctx: &Ctx,
    type_name: TypeName,
) -> UnaryExpression {
    UnaryExpression::SizeOfTypenameExpression(type_name)
}
pub fn unary_expression_array_access_expression(
    _ctx: &Ctx,
    assumed_array: UnaryExpression,
    index: Expression,
) -> UnaryExpression {
    UnaryExpression::ArrayAccessExpression(ArrayAccessExpression {
        assumed_array: Box::new(assumed_array),
        index,
    })
}
pub fn unary_expression_function_call_expression(
    _ctx: &Ctx,
    assumed_callable: UnaryExpression,
    arguments: ArgumentExpressionListOpt,
) -> UnaryExpression {
    UnaryExpression::FunctionCallExpression(FunctionCallExpression {
        assumed_callable: Box::new(assumed_callable),
        arguments,
    })
}
pub fn unary_expression_access_expression(
    _ctx: &Ctx,
    assumed_variable: UnaryExpression,
    identifier: Identifier,
) -> UnaryExpression {
    UnaryExpression::AccessExpression(AccessExpression {
        assumed_variable: Box::new(assumed_variable),
        identifier,
    })
}
pub fn unary_expression_pointer_access_expression(
    _ctx: &Ctx,
    assumed_pointer: UnaryExpression,
    identifier: Identifier,
) -> UnaryExpression {
    UnaryExpression::PointerAccessExpression(PointerAccessExpression {
        assumed_pointer: Box::new(assumed_pointer),
        identifier,
    })
}
pub fn unary_expression_post_increment_expression(
    _ctx: &Ctx,
    assumed_increment: UnaryExpression,
) -> UnaryExpression {
    UnaryExpression::PostIncrementExpression(PostIncrementExpression {
        assumed_increment: Box::new(assumed_increment),
    })
}
pub fn unary_expression_post_decrement_expression(
    _ctx: &Ctx,
    assumed_decrement: UnaryExpression,
) -> UnaryExpression {
    UnaryExpression::PostDecrementExpression(PostDecrementExpression {
        assumed_decrement: Box::new(assumed_decrement),
    })
}
pub fn unary_expression_compound_literal_expression(
    _ctx: &Ctx,
    type_cast: TypeCast,
    initializer_list: InitializerList,
    comma_opt: CommaOpt,
) -> UnaryExpression {
    UnaryExpression::CompoundLiteralExpression(CompoundLiteralExpression {
        type_cast,
        initializer_list,
        comma_opt,
    })
}
pub type TypeCastOpt = Option<TypeCast>;
pub fn type_cast_opt_type_cast(_ctx: &Ctx, type_cast: TypeCast) -> TypeCastOpt {
    Some(type_cast)
}
pub fn type_cast_opt_empty(_ctx: &Ctx) -> TypeCastOpt {
    None
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
pub enum PrimaryExpression {
    Number(Num),
    Identifier(Identifier),
    StringLiteral(StringLiteral),
    Paren(Expression),
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
pub fn primary_expression_string_literal(
    _ctx: &Ctx,
    string_literal: StringLiteral,
) -> PrimaryExpression {
    PrimaryExpression::StringLiteral(string_literal)
}
pub fn primary_expression_paren(
    _ctx: &Ctx,
    expression: Expression,
) -> PrimaryExpression {
    PrimaryExpression::Paren(expression)
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
    pub specifier_qualifier_kind1: SpecifierQualifierKind1,
    pub abstract_declarator_opt: AbstractDeclaratorOpt,
}
pub fn type_name_c1(
    _ctx: &Ctx,
    specifier_qualifier_kind1: SpecifierQualifierKind1,
    abstract_declarator_opt: AbstractDeclaratorOpt,
) -> TypeName {
    TypeName {
        specifier_qualifier_kind1,
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
    pub type_qualifier0: TypeQualifier0,
    pub assignment_expression_opt: Box<AssignmentExpressionOpt>,
}
#[derive(Debug, Clone)]
pub struct DirectAbstractDeclaratorC3 {
    pub direct_abstract_declarator_opt: DirectAbstractDeclaratorOpt,
    pub type_qualifier0: TypeQualifier0,
    pub assignment_expression: Box<AssignmentExpression>,
}
#[derive(Debug, Clone)]
pub struct DirectAbstractDeclaratorC4 {
    pub direct_abstract_declarator_opt: DirectAbstractDeclaratorOpt,
    pub type_qualifier1: TypeQualifier1,
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
    type_qualifier0: TypeQualifier0,
    assignment_expression_opt: AssignmentExpressionOpt,
) -> DirectAbstractDeclarator {
    DirectAbstractDeclarator::C2(DirectAbstractDeclaratorC2 {
        direct_abstract_declarator_opt,
        type_qualifier0,
        assignment_expression_opt: Box::new(assignment_expression_opt),
    })
}
pub fn direct_abstract_declarator_c3(
    _ctx: &Ctx,
    direct_abstract_declarator_opt: DirectAbstractDeclaratorOpt,
    type_qualifier0: TypeQualifier0,
    assignment_expression: AssignmentExpression,
) -> DirectAbstractDeclarator {
    DirectAbstractDeclarator::C3(DirectAbstractDeclaratorC3 {
        direct_abstract_declarator_opt,
        type_qualifier0,
        assignment_expression: Box::new(assignment_expression),
    })
}
pub fn direct_abstract_declarator_c4(
    _ctx: &Ctx,
    direct_abstract_declarator_opt: DirectAbstractDeclaratorOpt,
    type_qualifier1: TypeQualifier1,
    assignment_expression: AssignmentExpression,
) -> DirectAbstractDeclarator {
    DirectAbstractDeclarator::C4(DirectAbstractDeclaratorC4 {
        direct_abstract_declarator_opt,
        type_qualifier1,
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
pub enum ParameterDeclarationKind {
    Declarator(Box<Declarator>),
    AbstractDeclaratorOpt(Box<AbstractDeclaratorOpt>),
}
pub fn parameter_declaration_kind_declarator(
    _ctx: &Ctx,
    declarator: Declarator,
) -> ParameterDeclarationKind {
    ParameterDeclarationKind::Declarator(Box::new(declarator))
}
pub fn parameter_declaration_kind_abstract_declarator_opt(
    _ctx: &Ctx,
    abstract_declarator_opt: AbstractDeclaratorOpt,
) -> ParameterDeclarationKind {
    ParameterDeclarationKind::AbstractDeclaratorOpt(Box::new(abstract_declarator_opt))
}
#[derive(Debug, Clone)]
pub struct ParameterDeclaration {
    pub declaration_specifier1: Box<DeclarationSpecifier1>,
    pub kind: ParameterDeclarationKind,
}
pub fn parameter_declaration_c1(
    _ctx: &Ctx,
    declaration_specifier1: DeclarationSpecifier1,
    kind: ParameterDeclarationKind,
) -> ParameterDeclaration {
    ParameterDeclaration {
        declaration_specifier1: Box::new(declaration_specifier1),
        kind,
    }
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
pub type Designation = Designator1;
pub fn designation_designator1(_ctx: &Ctx, designator1: Designator1) -> Designation {
    designator1
}
pub type Designator1 = Vec<Designator>;
pub fn designator1_c1(
    _ctx: &Ctx,
    mut designator1: Designator1,
    designator: Designator,
) -> Designator1 {
    designator1.push(designator);
    designator1
}
pub fn designator1_designator(_ctx: &Ctx, designator: Designator) -> Designator1 {
    vec![designator]
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
pub type ConstantExpression = Box<ConditionalExpression>;
pub fn constant_expression_conditional_expression(
    _ctx: &Ctx,
    conditional_expression: ConditionalExpression,
) -> ConstantExpression {
    Box::new(conditional_expression)
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
pub struct Declaration {
    pub declaration_specifier1: DeclarationSpecifier1,
    pub init_declarator_list_opt: InitDeclaratorListOpt,
}
pub fn declaration_c1(
    _ctx: &Ctx,
    declaration_specifier1: DeclarationSpecifier1,
    init_declarator_list_opt: InitDeclaratorListOpt,
) -> Declaration {
    Declaration {
        declaration_specifier1,
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
#[derive(Debug, Clone)]
pub struct CompoundStatement {
    pub statements: BlockItem0,
}
pub fn compound_statement_c1(_ctx: &Ctx, statements: BlockItem0) -> CompoundStatement {
    CompoundStatement { statements }
}
pub type BlockItem1 = Vec<BlockItem>;
pub fn block_item1_c1(
    _ctx: &Ctx,
    mut block_item1: BlockItem1,
    block_item: BlockItem,
) -> BlockItem1 {
    block_item1.push(block_item);
    block_item1
}
pub fn block_item1_block_item(_ctx: &Ctx, block_item: BlockItem) -> BlockItem1 {
    vec![block_item]
}
pub type BlockItem0 = Option<BlockItem1>;
pub fn block_item0_block_item1(_ctx: &Ctx, block_item1: BlockItem1) -> BlockItem0 {
    Some(block_item1)
}
pub fn block_item0_empty(_ctx: &Ctx) -> BlockItem0 {
    None
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
pub struct LabelStatement {
    pub label: Identifier,
    pub body: Box<Statement>,
}
#[derive(Debug, Clone)]
pub struct CaseClause {
    pub condition: ConstantExpression,
    pub body: Box<Statement>,
}
#[derive(Debug, Clone)]
pub struct DefaultClause {
    pub body: Box<Statement>,
}
#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub expression: ExpressionOpt,
}
#[derive(Debug, Clone)]
pub struct IfStatement {
    pub condition: Expression,
    pub body: Box<Statement>,
}
#[derive(Debug, Clone)]
pub struct IfElseStatement {
    pub condition: Expression,
    pub branch_true: Box<Statement>,
    pub branch_false: Box<Statement>,
}
#[derive(Debug, Clone)]
pub struct SwitchStatement {
    pub condition: Expression,
    pub body: Box<Statement>,
}
#[derive(Debug, Clone)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Box<Statement>,
}
#[derive(Debug, Clone)]
pub struct DoWhileStatement {
    pub body: Box<Statement>,
    pub condition: Expression,
}
#[derive(Debug, Clone)]
pub struct ForStatement {
    pub init: ForInitClause,
    pub condition: ExpressionOpt,
    pub step: ExpressionOpt,
    pub body: Box<Statement>,
}
#[derive(Debug, Clone)]
pub struct GotoStatement {
    pub label: Identifier,
}
#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub value: ExpressionOpt,
}
#[derive(Debug, Clone)]
pub enum Statement {
    LabelStatement(LabelStatement),
    CaseClause(CaseClause),
    DefaultClause(DefaultClause),
    CompoundStatement(Box<CompoundStatement>),
    ExpressionStatement(ExpressionStatement),
    IfStatement(IfStatement),
    IfElseStatement(IfElseStatement),
    SwitchStatement(SwitchStatement),
    WhileStatement(WhileStatement),
    DoWhileStatement(DoWhileStatement),
    ForStatement(ForStatement),
    GotoStatement(GotoStatement),
    ContinueStatement,
    BreakStatement,
    ReturnStatement(ReturnStatement),
}
pub fn statement_label_statement(
    _ctx: &Ctx,
    label: Identifier,
    body: Statement,
) -> Statement {
    Statement::LabelStatement(LabelStatement {
        label,
        body: Box::new(body),
    })
}
pub fn statement_case_clause(
    _ctx: &Ctx,
    condition: ConstantExpression,
    body: Statement,
) -> Statement {
    Statement::CaseClause(CaseClause {
        condition,
        body: Box::new(body),
    })
}
pub fn statement_default_clause(_ctx: &Ctx, body: Statement) -> Statement {
    Statement::DefaultClause(DefaultClause {
        body: Box::new(body),
    })
}
pub fn statement_compound_statement(
    _ctx: &Ctx,
    compound_statement: CompoundStatement,
) -> Statement {
    Statement::CompoundStatement(Box::new(compound_statement))
}
pub fn statement_expression_statement(
    _ctx: &Ctx,
    expression: ExpressionOpt,
) -> Statement {
    Statement::ExpressionStatement(ExpressionStatement { expression })
}
pub fn statement_if_statement(
    _ctx: &Ctx,
    condition: Expression,
    body: Statement,
) -> Statement {
    Statement::IfStatement(IfStatement {
        condition,
        body: Box::new(body),
    })
}
pub fn statement_if_else_statement(
    _ctx: &Ctx,
    condition: Expression,
    branch_true: Statement,
    branch_false: Statement,
) -> Statement {
    Statement::IfElseStatement(IfElseStatement {
        condition,
        branch_true: Box::new(branch_true),
        branch_false: Box::new(branch_false),
    })
}
pub fn statement_switch_statement(
    _ctx: &Ctx,
    condition: Expression,
    body: Statement,
) -> Statement {
    Statement::SwitchStatement(SwitchStatement {
        condition,
        body: Box::new(body),
    })
}
pub fn statement_while_statement(
    _ctx: &Ctx,
    condition: Expression,
    body: Statement,
) -> Statement {
    Statement::WhileStatement(WhileStatement {
        condition,
        body: Box::new(body),
    })
}
pub fn statement_do_while_statement(
    _ctx: &Ctx,
    body: Statement,
    condition: Expression,
) -> Statement {
    Statement::DoWhileStatement(DoWhileStatement {
        body: Box::new(body),
        condition,
    })
}
pub fn statement_for_statement(
    _ctx: &Ctx,
    init: ForInitClause,
    condition: ExpressionOpt,
    step: ExpressionOpt,
    body: Statement,
) -> Statement {
    Statement::ForStatement(ForStatement {
        init,
        condition,
        step,
        body: Box::new(body),
    })
}
pub fn statement_goto_statement(_ctx: &Ctx, label: Identifier) -> Statement {
    Statement::GotoStatement(GotoStatement { label })
}
pub fn statement_continue_statement(_ctx: &Ctx) -> Statement {
    Statement::ContinueStatement
}
pub fn statement_break_statement(_ctx: &Ctx) -> Statement {
    Statement::BreakStatement
}
pub fn statement_return_statement(_ctx: &Ctx, value: ExpressionOpt) -> Statement {
    Statement::ReturnStatement(ReturnStatement { value })
}
pub type ExpressionOpt = Option<Expression>;
pub fn expression_opt_expression(_ctx: &Ctx, expression: Expression) -> ExpressionOpt {
    Some(expression)
}
pub fn expression_opt_empty(_ctx: &Ctx) -> ExpressionOpt {
    None
}
#[derive(Debug, Clone)]
pub enum ForInitClause {
    None,
    ForInitWithExpression(Expression),
    ForInitWithDeclaration(Declaration),
}
pub fn for_init_clause_none(_ctx: &Ctx) -> ForInitClause {
    ForInitClause::None
}
pub fn for_init_clause_for_init_with_expression(
    _ctx: &Ctx,
    expression: Expression,
) -> ForInitClause {
    ForInitClause::ForInitWithExpression(expression)
}
pub fn for_init_clause_for_init_with_declaration(
    _ctx: &Ctx,
    declaration: Declaration,
) -> ForInitClause {
    ForInitClause::ForInitWithDeclaration(declaration)
}
