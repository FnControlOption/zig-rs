//! Fully assembled AST node information.

use super::*;

impl<'src> Ast<'src> {
    pub fn global_var_decl(&self, node: node::Index) -> VarDecl {
        debug_assert_eq!(self.node(node).tag, N::GlobalVarDecl);
        let node::Data { lhs, rhs } = self.node(node).data;
        let extra: node::GlobalVarDecl = self.extra_data(lhs);
        self.full_var_decl_components(VarDeclComponents {
            mut_token: self.node(node).main_token,
            type_node: extra.type_node,
            align_node: extra.align_node,
            addrspace_node: extra.addrspace_node,
            section_node: extra.section_node,
            init_node: rhs,
        })
    }

    pub fn local_var_decl(&self, node: node::Index) -> VarDecl {
        debug_assert_eq!(self.node(node).tag, N::LocalVarDecl);
        let node::Data { lhs, rhs } = self.node(node).data;
        let extra: node::LocalVarDecl = self.extra_data(lhs);
        self.full_var_decl_components(VarDeclComponents {
            mut_token: self.node(node).main_token,
            type_node: extra.type_node,
            align_node: extra.align_node,
            addrspace_node: 0,
            section_node: 0,
            init_node: rhs,
        })
    }

    pub fn simple_var_decl(&self, node: node::Index) -> VarDecl {
        debug_assert_eq!(self.node(node).tag, N::SimpleVarDecl);
        let node::Data { lhs, rhs } = self.node(node).data;
        self.full_var_decl_components(VarDeclComponents {
            mut_token: self.node(node).main_token,
            type_node: lhs,
            align_node: 0,
            addrspace_node: 0,
            section_node: 0,
            init_node: rhs,
        })
    }

    pub fn aligned_var_decl(&self, node: node::Index) -> VarDecl {
        debug_assert_eq!(self.node(node).tag, N::AlignedVarDecl);
        let node::Data { lhs, rhs } = self.node(node).data;
        self.full_var_decl_components(VarDeclComponents {
            mut_token: self.node(node).main_token,
            type_node: 0,
            align_node: lhs,
            addrspace_node: 0,
            section_node: 0,
            init_node: rhs,
        })
    }

    pub fn if_simple(&self, node: node::Index) -> If {
        debug_assert_eq!(self.node(node).tag, N::IfSimple);
        let node::Data { lhs, rhs } = self.node(node).data;
        self.full_if_components(IfComponents {
            cond_expr: lhs,
            then_expr: rhs,
            else_expr: 0,
            if_token: self.node(node).main_token,
        })
    }

    pub fn if_full(&self, node: node::Index) -> If {
        debug_assert_eq!(self.node(node).tag, N::If);
        let node::Data { lhs, rhs } = self.node(node).data;
        let extra: node::If = self.extra_data(rhs);
        self.full_if_components(IfComponents {
            cond_expr: lhs,
            then_expr: extra.then_expr,
            else_expr: extra.else_expr,
            if_token: self.node(node).main_token,
        })
    }

    pub fn container_field(&self, node: node::Index) -> ContainerField {
        debug_assert_eq!(self.node(node).tag, N::ContainerField);
        let node::Data { lhs, rhs } = self.node(node).data;
        let extra: node::ContainerField = self.extra_data(rhs);
        let main_token = self.node(node).main_token;
        self.full_container_field_components(ContainerFieldComponents {
            main_token,
            type_expr: lhs,
            align_expr: extra.align_expr,
            value_expr: extra.value_expr,
            tuple_like: self.token_tag(main_token) != T::Identifier
                || self.token_tag(main_token + 1) != T::Colon,
        })
    }

    pub fn container_field_init(&self, node: node::Index) -> ContainerField {
        debug_assert_eq!(self.node(node).tag, N::ContainerFieldInit);
        let node::Data { lhs, rhs } = self.node(node).data;
        let main_token = self.node(node).main_token;
        self.full_container_field_components(ContainerFieldComponents {
            main_token,
            type_expr: lhs,
            align_expr: 0,
            value_expr: rhs,
            tuple_like: self.token_tag(main_token) != T::Identifier
                || self.token_tag(main_token + 1) != T::Colon,
        })
    }

    pub fn container_field_align(&self, node: node::Index) -> ContainerField {
        debug_assert_eq!(self.node(node).tag, N::ContainerFieldAlign);
        let node::Data { lhs, rhs } = self.node(node).data;
        let main_token = self.node(node).main_token;
        self.full_container_field_components(ContainerFieldComponents {
            main_token,
            type_expr: lhs,
            align_expr: rhs,
            value_expr: 0,
            tuple_like: self.token_tag(main_token) != T::Identifier
                || self.token_tag(main_token + 1) != T::Colon,
        })
    }

    pub fn fn_proto_simple<'ast, 'buf>(
        &'ast self,
        buffer: &'buf mut [node::Index; 1],
        node: node::Index,
    ) -> FnProto<'buf>
    where
        'ast: 'buf,
    {
        debug_assert_eq!(self.node(node).tag, N::FnProtoSimple);
        let node::Data { lhs, rhs } = self.node(node).data;
        *buffer = [lhs];
        let params = match lhs {
            0 => &buffer[0..0],
            _ => &buffer[0..1],
        };
        self.full_fn_proto_components(FnProtoComponents {
            proto_node: node,
            fn_token: self.node(node).main_token,
            return_type: rhs,
            params,
            align_expr: 0,
            addrspace_expr: 0,
            section_expr: 0,
            callconv_expr: 0,
        })
    }

    pub fn fn_proto_multi(&self, node: node::Index) -> FnProto {
        debug_assert_eq!(self.node(node).tag, N::FnProtoMulti);
        let node::Data { lhs, rhs } = self.node(node).data;
        let params_range: node::SubRange = self.extra_data(lhs);
        let params = self.extra_data(params_range.start..params_range.end);
        self.full_fn_proto_components(FnProtoComponents {
            proto_node: node,
            fn_token: self.node(node).main_token,
            return_type: rhs,
            params,
            align_expr: 0,
            addrspace_expr: 0,
            section_expr: 0,
            callconv_expr: 0,
        })
    }

    pub fn fn_proto_one<'ast, 'buf>(
        &'ast self,
        buffer: &'buf mut [node::Index; 1],
        node: node::Index,
    ) -> FnProto<'buf>
    where
        'ast: 'buf,
    {
        debug_assert_eq!(self.node(node).tag, N::FnProtoOne);
        let node::Data { lhs, rhs } = self.node(node).data;
        let extra: node::FnProtoOne = self.extra_data(lhs);
        *buffer = [extra.param];
        let params = match extra.param {
            0 => &buffer[0..0],
            _ => &buffer[0..1],
        };
        self.full_fn_proto_components(FnProtoComponents {
            proto_node: node,
            fn_token: self.node(node).main_token,
            return_type: rhs,
            params,
            align_expr: extra.align_expr,
            addrspace_expr: extra.addrspace_expr,
            section_expr: extra.section_expr,
            callconv_expr: extra.callconv_expr,
        })
    }

    pub fn fn_proto(&self, node: node::Index) -> FnProto {
        debug_assert_eq!(self.node(node).tag, N::FnProto);
        let node::Data { lhs, rhs } = self.node(node).data;
        let extra: node::FnProto = self.extra_data(lhs);
        let params = self.extra_data(extra.params_start..extra.params_end);
        self.full_fn_proto_components(FnProtoComponents {
            proto_node: node,
            fn_token: self.node(node).main_token,
            return_type: rhs,
            params,
            align_expr: extra.align_expr,
            addrspace_expr: extra.addrspace_expr,
            section_expr: extra.section_expr,
            callconv_expr: extra.callconv_expr,
        })
    }

    pub fn struct_init_one<'ast, 'buf>(
        &'ast self,
        buffer: &'buf mut [node::Index; 1],
        node: node::Index,
    ) -> StructInit<'buf>
    where
        'ast: 'buf,
    {
        debug_assert!(matches!(
            self.node(node).tag,
            N::StructInitOne | N::StructInitOneComma
        ));
        let node::Data { lhs, rhs } = self.node(node).data;
        *buffer = [rhs];
        let fields = match rhs {
            0 => &buffer[0..0],
            _ => &buffer[0..1],
        };
        StructInit {
            ast: StructInitComponents {
                lbrace: self.node(node).main_token,
                fields,
                type_expr: lhs,
            },
        }
    }

    pub fn struct_init_dot_two<'ast, 'buf>(
        &'ast self,
        buffer: &'buf mut [node::Index; 2],
        node: node::Index,
    ) -> StructInit<'buf>
    where
        'ast: 'buf,
    {
        debug_assert!(matches!(
            self.node(node).tag,
            N::StructInitDotTwo | N::StructInitDotTwoComma
        ));
        let node::Data { lhs, rhs } = self.node(node).data;
        *buffer = [lhs, rhs];
        let fields = match [lhs, rhs] {
            [0, 0] => &buffer[0..0],
            [_, 0] => &buffer[0..1],
            [_, _] => &buffer[0..2],
        };
        StructInit {
            ast: StructInitComponents {
                lbrace: self.node(node).main_token,
                fields,
                type_expr: 0,
            },
        }
    }

    pub fn struct_init_dot(&self, node: node::Index) -> StructInit {
        debug_assert!(matches!(
            self.node(node).tag,
            N::StructInitDot | N::StructInitDotComma
        ));
        let node::Data { lhs, rhs } = self.node(node).data;
        StructInit {
            ast: StructInitComponents {
                lbrace: self.node(node).main_token,
                fields: self.extra_data(lhs..rhs),
                type_expr: 0,
            },
        }
    }

    pub fn struct_init(&self, node: node::Index) -> StructInit {
        debug_assert!(matches!(
            self.node(node).tag,
            N::StructInit | N::StructInitComma
        ));
        let node::Data { lhs, rhs } = self.node(node).data;
        let fields_range: node::SubRange = self.extra_data(rhs);
        StructInit {
            ast: StructInitComponents {
                lbrace: self.node(node).main_token,
                fields: self.extra_data(fields_range.start..fields_range.end),
                type_expr: lhs,
            },
        }
    }

    pub fn array_init_one<'ast, 'buf>(
        &'ast self,
        buffer: &'buf mut [node::Index; 1],
        node: node::Index,
    ) -> ArrayInit<'buf>
    where
        'ast: 'buf,
    {
        debug_assert!(matches!(
            self.node(node).tag,
            N::ArrayInitOne | N::ArrayInitOneComma
        ));
        let node::Data { lhs, rhs } = self.node(node).data;
        *buffer = [rhs];
        let elements = match rhs {
            0 => &buffer[0..0],
            _ => &buffer[0..1],
        };
        ArrayInit {
            ast: ArrayInitComponents {
                lbrace: self.node(node).main_token,
                elements,
                type_expr: lhs,
            },
        }
    }

    pub fn array_init_dot_two<'ast, 'buf>(
        &'ast self,
        buffer: &'buf mut [node::Index; 2],
        node: node::Index,
    ) -> ArrayInit<'buf>
    where
        'ast: 'buf,
    {
        debug_assert!(matches!(
            self.node(node).tag,
            N::ArrayInitDotTwo | N::ArrayInitDotTwoComma
        ));
        let node::Data { lhs, rhs } = self.node(node).data;
        *buffer = [lhs, rhs];
        let elements = match [lhs, rhs] {
            [0, 0] => &buffer[0..0],
            [_, 0] => &buffer[0..1],
            [_, _] => &buffer[0..2],
        };
        ArrayInit {
            ast: ArrayInitComponents {
                lbrace: self.node(node).main_token,
                elements,
                type_expr: 0,
            },
        }
    }

    pub fn array_init_dot(&self, node: node::Index) -> ArrayInit {
        debug_assert!(matches!(
            self.node(node).tag,
            N::ArrayInitDot | N::ArrayInitDotComma
        ));
        let node::Data { lhs, rhs } = self.node(node).data;
        ArrayInit {
            ast: ArrayInitComponents {
                lbrace: self.node(node).main_token,
                elements: self.extra_data(lhs..rhs),
                type_expr: 0,
            },
        }
    }

    pub fn array_init(&self, node: node::Index) -> ArrayInit {
        debug_assert!(matches!(
            self.node(node).tag,
            N::ArrayInit | N::ArrayInitComma
        ));
        let node::Data { lhs, rhs } = self.node(node).data;
        let elem_range: node::SubRange = self.extra_data(rhs);
        ArrayInit {
            ast: ArrayInitComponents {
                lbrace: self.node(node).main_token,
                elements: self.extra_data(elem_range.start..elem_range.end),
                type_expr: lhs,
            },
        }
    }

    pub fn array_type(&self, node: node::Index) -> ArrayType {
        debug_assert_eq!(self.node(node).tag, N::ArrayType);
        let node::Data { lhs, rhs } = self.node(node).data;
        ArrayType {
            ast: ArrayTypeComponents {
                lbracket: self.node(node).main_token,
                elem_count: lhs,
                sentinel: 0,
                elem_type: rhs,
            },
        }
    }

    pub fn array_type_sentinel(&self, node: node::Index) -> ArrayType {
        debug_assert_eq!(self.node(node).tag, N::ArrayTypeSentinel);
        let node::Data { lhs, rhs } = self.node(node).data;
        let extra: node::ArrayTypeSentinel = self.extra_data(rhs);
        debug_assert_ne!(extra.sentinel, 0);
        ArrayType {
            ast: ArrayTypeComponents {
                lbracket: self.node(node).main_token,
                elem_count: lhs,
                sentinel: extra.sentinel,
                elem_type: extra.elem_type,
            },
        }
    }

    pub fn ptr_type_aligned(&self, node: node::Index) -> PtrType {
        debug_assert_eq!(self.node(node).tag, N::PtrTypeAligned);
        let node::Data { lhs, rhs } = self.node(node).data;
        self.full_ptr_type_components(PtrTypeComponents {
            main_token: self.node(node).main_token,
            align_node: lhs,
            addrspace_node: 0,
            sentinel: 0,
            bit_range_start: 0,
            bit_range_end: 0,
            child_type: rhs,
        })
    }

    pub fn ptr_type_sentinel(&self, node: node::Index) -> PtrType {
        debug_assert_eq!(self.node(node).tag, N::PtrTypeSentinel);
        let node::Data { lhs, rhs } = self.node(node).data;
        self.full_ptr_type_components(PtrTypeComponents {
            main_token: self.node(node).main_token,
            align_node: 0,
            addrspace_node: 0,
            sentinel: lhs,
            bit_range_start: 0,
            bit_range_end: 0,
            child_type: rhs,
        })
    }

    pub fn ptr_type(&self, node: node::Index) -> PtrType {
        debug_assert_eq!(self.node(node).tag, N::PtrType);
        let node::Data { lhs, rhs } = self.node(node).data;
        let extra: node::PtrType = self.extra_data(lhs);
        self.full_ptr_type_components(PtrTypeComponents {
            main_token: self.node(node).main_token,
            align_node: extra.align_node,
            addrspace_node: extra.addrspace_node,
            sentinel: extra.sentinel,
            bit_range_start: 0,
            bit_range_end: 0,
            child_type: rhs,
        })
    }

    pub fn ptr_type_bit_range(&self, node: node::Index) -> PtrType {
        debug_assert_eq!(self.node(node).tag, N::PtrTypeBitRange);
        let node::Data { lhs, rhs } = self.node(node).data;
        let extra: node::PtrTypeBitRange = self.extra_data(lhs);
        self.full_ptr_type_components(PtrTypeComponents {
            main_token: self.node(node).main_token,
            align_node: extra.align_node,
            addrspace_node: extra.addrspace_node,
            sentinel: extra.sentinel,
            bit_range_start: extra.bit_range_start,
            bit_range_end: extra.bit_range_end,
            child_type: rhs,
        })
    }

    pub fn slice_open(&self, node: node::Index) -> Slice {
        debug_assert_eq!(self.node(node).tag, N::SliceOpen);
        let node::Data { lhs, rhs } = self.node(node).data;
        Slice {
            ast: SliceComponents {
                sliced: lhs,
                lbracket: self.node(node).main_token,
                start: rhs,
                end: 0,
                sentinel: 0,
            },
        }
    }

    pub fn slice(&self, node: node::Index) -> Slice {
        debug_assert_eq!(self.node(node).tag, N::Slice);
        let node::Data { lhs, rhs } = self.node(node).data;
        let extra: node::Slice = self.extra_data(rhs);
        Slice {
            ast: SliceComponents {
                sliced: lhs,
                lbracket: self.node(node).main_token,
                start: extra.start,
                end: extra.end,
                sentinel: 0,
            },
        }
    }

    pub fn slice_sentinel(&self, node: node::Index) -> Slice {
        debug_assert_eq!(self.node(node).tag, N::SliceSentinel);
        let node::Data { lhs, rhs } = self.node(node).data;
        let extra: node::SliceSentinel = self.extra_data(rhs);
        Slice {
            ast: SliceComponents {
                sliced: lhs,
                lbracket: self.node(node).main_token,
                start: extra.start,
                end: extra.end,
                sentinel: extra.sentinel,
            },
        }
    }

    pub fn container_decl_two<'ast, 'buf>(
        &'ast self,
        buffer: &'buf mut [node::Index; 2],
        node: node::Index,
    ) -> ContainerDecl<'buf>
    where
        'ast: 'buf,
    {
        debug_assert!(matches!(
            self.node(node).tag,
            N::ContainerDeclTwo | N::ContainerDeclTwoTrailing
        ));
        let node::Data { lhs, rhs } = self.node(node).data;
        *buffer = [lhs, rhs];
        let members = match [lhs, rhs] {
            [0, 0] => &buffer[0..0],
            [_, 0] => &buffer[0..1],
            [_, _] => &buffer[0..2],
        };
        self.full_container_decl_components(ContainerDeclComponents {
            main_token: self.node(node).main_token,
            enum_token: None,
            members,
            arg: 0,
        })
    }

    pub fn container_decl(&self, node: node::Index) -> ContainerDecl {
        debug_assert!(matches!(
            self.node(node).tag,
            N::ContainerDecl | N::ContainerDeclTrailing
        ));
        let node::Data { lhs, rhs } = self.node(node).data;
        self.full_container_decl_components(ContainerDeclComponents {
            main_token: self.node(node).main_token,
            enum_token: None,
            members: self.extra_data(lhs..rhs),
            arg: 0,
        })
    }

    pub fn container_decl_arg(&self, node: node::Index) -> ContainerDecl {
        debug_assert!(matches!(
            self.node(node).tag,
            N::ContainerDeclArg | N::ContainerDeclArgTrailing
        ));
        let node::Data { lhs, rhs } = self.node(node).data;
        let members_range: node::SubRange = self.extra_data(rhs);
        self.full_container_decl_components(ContainerDeclComponents {
            main_token: self.node(node).main_token,
            enum_token: None,
            members: self.extra_data(members_range.start..members_range.end),
            arg: lhs,
        })
    }

    pub fn container_decl_root(&self) -> ContainerDecl {
        ContainerDecl {
            layout_token: None,
            ast: ContainerDeclComponents {
                main_token: UNDEFINED_TOKEN,
                enum_token: None,
                members: self.root_decls(),
                arg: 0,
            },
        }
    }

    pub fn tagged_union_two<'ast, 'buf>(
        &'ast self,
        buffer: &'buf mut [node::Index; 2],
        node: node::Index,
    ) -> ContainerDecl<'buf>
    where
        'ast: 'buf,
    {
        debug_assert!(matches!(
            self.node(node).tag,
            N::TaggedUnionTwo | N::TaggedUnionTwoTrailing
        ));
        let node::Data { lhs, rhs } = self.node(node).data;
        *buffer = [lhs, rhs];
        let members = match [lhs, rhs] {
            [0, 0] => &buffer[0..0],
            [_, 0] => &buffer[0..1],
            [_, _] => &buffer[0..2],
        };
        let main_token = self.node(node).main_token;
        self.full_container_decl_components(ContainerDeclComponents {
            main_token,
            enum_token: Some(main_token + 2), // union lparen enum
            members,
            arg: 0,
        })
    }

    pub fn tagged_union(&self, node: node::Index) -> ContainerDecl {
        debug_assert!(matches!(
            self.node(node).tag,
            N::TaggedUnion | N::TaggedUnionTrailing
        ));
        let node::Data { lhs, rhs } = self.node(node).data;
        let main_token = self.node(node).main_token;
        self.full_container_decl_components(ContainerDeclComponents {
            main_token,
            enum_token: Some(main_token + 2), // union lparen enum
            members: self.extra_data(lhs..rhs),
            arg: 0,
        })
    }

    pub fn tagged_union_enum_tag(&self, node: node::Index) -> ContainerDecl {
        debug_assert!(matches!(
            self.node(node).tag,
            N::TaggedUnionEnumTag | N::TaggedUnionEnumTagTrailing
        ));
        let node::Data { lhs, rhs } = self.node(node).data;
        let members_range: node::SubRange = self.extra_data(rhs);
        let main_token = self.node(node).main_token;
        self.full_container_decl_components(ContainerDeclComponents {
            main_token,
            enum_token: Some(main_token + 2),
            members: self.extra_data(members_range.start..members_range.end),
            arg: lhs,
        })
    }

    pub fn switch_case_one(&self, node: node::Index) -> SwitchCase {
        let n = self.node(node);
        let values = std::slice::from_ref(&n.data.lhs);
        self.full_switch_case_components(
            SwitchCaseComponents {
                values,
                arrow_token: n.main_token,
                target_expr: n.data.rhs,
            },
            node,
        )
    }

    pub fn switch_case(&self, node: node::Index) -> SwitchCase {
        let node::Data { lhs, rhs } = self.node(node).data;
        let extra: node::SubRange = self.extra_data(lhs);
        self.full_switch_case_components(
            SwitchCaseComponents {
                values: self.extra_data(extra.start..extra.end),
                arrow_token: self.node(node).main_token,
                target_expr: rhs,
            },
            node,
        )
    }

    pub fn asm_simple(&self, node: node::Index) -> Asm {
        let node::Data { lhs, rhs } = self.node(node).data;
        self.full_asm_components(AsmComponents {
            asm_token: self.node(node).main_token,
            template: lhs,
            items: &[],
            rparen: rhs,
        })
    }

    pub fn asm_full(&self, node: node::Index) -> Asm {
        let node::Data { lhs, rhs } = self.node(node).data;
        let extra: node::Asm = self.extra_data(rhs);
        self.full_asm_components(AsmComponents {
            asm_token: self.node(node).main_token,
            template: lhs,
            items: self.extra_data(extra.items_start..extra.items_end),
            rparen: extra.rparen,
        })
    }

    pub fn while_simple(&self, node: node::Index) -> While {
        let node::Data { lhs, rhs } = self.node(node).data;
        self.full_while_components(WhileComponents {
            while_token: self.node(node).main_token,
            cond_expr: lhs,
            cont_expr: 0,
            then_expr: rhs,
            else_expr: 0,
        })
    }

    pub fn while_cont(&self, node: node::Index) -> While {
        let node::Data { lhs, rhs } = self.node(node).data;
        let extra: node::WhileCont = self.extra_data(rhs);
        self.full_while_components(WhileComponents {
            while_token: self.node(node).main_token,
            cond_expr: lhs,
            cont_expr: extra.cont_expr,
            then_expr: extra.then_expr,
            else_expr: 0,
        })
    }

    pub fn while_full(&self, node: node::Index) -> While {
        let node::Data { lhs, rhs } = self.node(node).data;
        let extra: node::While = self.extra_data(rhs);
        self.full_while_components(WhileComponents {
            while_token: self.node(node).main_token,
            cond_expr: lhs,
            cont_expr: extra.cont_expr,
            then_expr: extra.then_expr,
            else_expr: extra.else_expr,
        })
    }

    pub fn for_simple(&self, node: node::Index) -> For {
        let n = self.node(node);
        let inputs = std::slice::from_ref(&n.data.lhs);
        self.full_for_components(ForComponents {
            for_token: n.main_token,
            inputs,
            then_expr: n.data.rhs,
            else_expr: 0,
        })
    }

    pub fn for_full(&self, node: node::Index) -> For {
        let node::Data { lhs, rhs } = self.node(node).data;
        let extra = node::For(self.extra_data(rhs));
        let inputs = self.extra_data(lhs..lhs + extra.inputs());
        let then_expr = self.extra_data(lhs + extra.inputs());
        let else_expr = match extra.has_else() {
            true => self.extra_data(lhs + extra.inputs() + 1),
            false => 0,
        };
        self.full_for_components(ForComponents {
            for_token: self.node(node).main_token,
            inputs,
            then_expr,
            else_expr,
        })
    }

    pub fn call_one<'ast, 'buf>(
        &'ast self,
        buffer: &'buf mut [node::Index; 1],
        node: node::Index,
    ) -> Call<'buf>
    where
        'ast: 'buf,
    {
        let node::Data { lhs, rhs } = self.node(node).data;
        *buffer = [rhs];
        let params = match rhs {
            0 => &buffer[0..0],
            _ => &buffer[0..1],
        };
        self.full_call_components(CallComponents {
            lparen: self.node(node).main_token,
            fn_expr: lhs,
            params,
        })
    }

    pub fn call_full(&self, node: node::Index) -> Call {
        let node::Data { lhs, rhs } = self.node(node).data;
        let extra: node::SubRange = self.extra_data(rhs);
        self.full_call_components(CallComponents {
            lparen: self.node(node).main_token,
            fn_expr: lhs,
            params: self.extra_data(extra.start..extra.end),
        })
    }

    fn full_var_decl_components(&self, info: VarDeclComponents) -> VarDecl {
        let mut result = VarDecl {
            ast: VarDeclComponents { ..info },
            visib_token: None,
            extern_export_token: None,
            lib_name: None,
            threadlocal_token: None,
            comptime_token: None,
        };
        let mut i = info.mut_token;
        while i > 0 {
            i -= 1;
            match self.token_tag(i) {
                T::KeywordExtern | T::KeywordExport => result.extern_export_token = Some(i),
                T::KeywordComptime => result.comptime_token = Some(i),
                T::KeywordPub => result.visib_token = Some(i),
                T::KeywordThreadlocal => result.threadlocal_token = Some(i),
                T::StringLiteral => result.lib_name = Some(i),
                _ => break,
            }
        }
        result
    }

    fn full_if_components(&self, info: IfComponents) -> If {
        let mut result = If {
            ast: IfComponents { ..info },
            payload_token: None,
            error_token: None,
            else_token: UNDEFINED_TOKEN,
        };

        // if (cond_expr) |x|
        //              ^ ^
        let payload_pipe = self.last_token(info.cond_expr) + 2;
        if self.token_tag(payload_pipe) == T::Pipe {
            result.payload_token = Some(payload_pipe + 1);
        }

        if info.else_expr != 0 {
            // then_expr else |x|
            //           ^    ^
            result.else_token = self.last_token(info.then_expr) + 1;
            if self.token_tag(result.else_token + 1) == T::Pipe {
                result.error_token = Some(result.else_token + 2);
            }
        }

        result
    }

    fn full_container_field_components(&self, info: ContainerFieldComponents) -> ContainerField {
        let mut result = ContainerField {
            ast: ContainerFieldComponents { ..info },
            comptime_token: None,
        };

        if self.token_tag(info.main_token) == T::KeywordComptime {
            // comptime type = init,
            // ^
            result.comptime_token = Some(info.main_token);
        } else if info.main_token > 0 && self.token_tag(info.main_token - 1) == T::KeywordComptime {
            // comptime name: type = init,
            // ^
            result.comptime_token = Some(info.main_token - 1);
        }

        result
    }

    fn full_fn_proto_components<'ast, 'buf>(
        &'ast self,
        info: FnProtoComponents<'buf>,
    ) -> FnProto<'buf>
    where
        'ast: 'buf,
    {
        let mut result = FnProto {
            ast: FnProtoComponents { ..info },
            visib_token: None,
            extern_export_inline_token: None,
            lib_name: None,
            name_token: None,
            lparen: UNDEFINED_TOKEN,
        };

        let mut i = info.fn_token;
        while i > 0 {
            i -= 1;
            match self.token_tag(i) {
                T::KeywordExtern | T::KeywordExport | T::KeywordInline | T::KeywordNoinline => {
                    result.extern_export_inline_token = Some(i)
                }
                T::KeywordPub => result.visib_token = Some(i),
                T::StringLiteral => result.lib_name = Some(i),
                _ => break,
            }
        }

        let after_fn_token = info.fn_token + 1;
        if self.token_tag(after_fn_token) == T::Identifier {
            result.name_token = Some(after_fn_token);
            result.lparen = after_fn_token + 1;
        } else {
            result.lparen = after_fn_token;
        }

        debug_assert_eq!(self.token_tag(result.lparen), T::LParen);

        result
    }

    fn full_ptr_type_components(&self, info: PtrTypeComponents) -> PtrType {
        let size = match self.token_tag(info.main_token) {
            T::Asterisk | T::AsteriskAsterisk => match self.token_tag(info.main_token + 1) {
                T::RBracket | T::Colon => PtrSize::Many,
                T::Identifier => match self.token_tag(info.main_token.saturating_sub(1)) {
                    T::LBracket => PtrSize::C,
                    _ => PtrSize::One,
                },
                _ => PtrSize::One,
            },
            T::LBracket => PtrSize::Slice,
            _ => unreachable!(),
        };
        let mut result = PtrType {
            size,
            allowzero_token: None,
            const_token: None,
            volatile_token: None,
            ast: PtrTypeComponents { ..info },
        };
        // We need to be careful that we don't iterate over any sub-expressions
        // here while looking for modifiers as that could result in false
        // positives. Therefore, start after a sentinel if there is one and
        // skip over any align node and bit range nodes.
        let mut i = match info.sentinel {
            0 => info.main_token,
            sentinel => self.last_token(sentinel) + 1,
        };
        let end = self.first_token(info.child_type);
        while i < end {
            match self.token_tag(i) {
                T::KeywordAllowzero => result.allowzero_token = Some(i),
                T::KeywordConst => result.const_token = Some(i),
                T::KeywordVolatile => result.volatile_token = Some(i),
                T::KeywordAlign => {
                    debug_assert_ne!(info.align_node, 0);
                    i = if info.bit_range_end != 0 {
                        debug_assert_ne!(info.bit_range_start, 0);
                        self.last_token(info.bit_range_end) + 1
                    } else {
                        self.last_token(info.align_node) + 1
                    };
                }
                _ => {}
            }
            i += 1;
        }
        result
    }

    fn full_container_decl_components<'ast, 'buf>(
        &'ast self,
        info: ContainerDeclComponents<'buf>,
    ) -> ContainerDecl<'buf>
    where
        'ast: 'buf,
    {
        let mut result = ContainerDecl {
            ast: ContainerDeclComponents { ..info },
            layout_token: None,
        };

        if info.main_token == 0 {
            return result;
        }

        match self.token_tag(info.main_token - 1) {
            T::KeywordExtern | T::KeywordPacked => result.layout_token = Some(info.main_token - 1),
            _ => {}
        }

        result
    }

    fn full_switch_case_components<'ast, 'buf>(
        &'ast self,
        info: SwitchCaseComponents<'buf>,
        node: node::Index,
    ) -> SwitchCase<'buf>
    where
        'ast: 'buf,
    {
        let mut result = SwitchCase {
            ast: SwitchCaseComponents { ..info },
            payload_token: None,
            inline_token: None,
        };

        if self.token_tag(info.arrow_token + 1) == T::Pipe {
            result.payload_token = Some(info.arrow_token + 2);
        }

        match self.node(node).tag {
            N::SwitchCaseInline | N::SwitchCaseInlineOne => {
                result.inline_token = Some(self.first_token(node))
            }
            _ => {}
        }

        result
    }

    fn full_asm_components<'ast, 'buf>(&'ast self, info: AsmComponents<'buf>) -> Asm<'buf>
    where
        'ast: 'buf,
    {
        let mut result = Asm {
            ast: AsmComponents { ..info },
            volatile_token: None,
            inputs: &[],
            outputs: &[],
            first_clobber: None,
        };

        if self.token_tag(info.asm_token + 1) == T::KeywordVolatile {
            result.volatile_token = Some(info.asm_token + 1);
        }

        let outputs_end = info
            .items
            .iter()
            .take_while(|&&item| self.node(item).tag == N::AsmOutput)
            .count();

        result.outputs = &info.items[..outputs_end];
        result.inputs = &info.items[outputs_end..];

        match info.items[..] {
            [] => {
                // asm ("foo" ::: "a", "b");
                let template_token = self.last_token(info.template);
                if self.token_tag(template_token + 1) == T::Colon
                    && self.token_tag(template_token + 2) == T::Colon
                    && self.token_tag(template_token + 3) == T::Colon
                    && self.token_tag(template_token + 4) == T::StringLiteral
                {
                    result.first_clobber = Some(template_token + 4);
                }
            }
            [.., last_output] => match result.inputs[..] {
                [.., last_input] => {
                    // asm ("foo" :: [_] "" (y) : "a", "b");
                    let rparen = self.last_token(last_input);
                    let mut i = rparen + 1;
                    // Allow a (useless) comma right after the closing parenthesis.
                    if self.token_tag(i) == T::Comma {
                        i += 1;
                    }
                    if self.token_tag(i) == T::Colon && self.token_tag(i + 1) == T::StringLiteral {
                        result.first_clobber = Some(i + 1);
                    }
                }
                [] => {
                    // asm ("foo" : [_] "" (x) :: "a", "b");
                    let rparen = self.last_token(last_output);
                    let mut i = rparen + 1;
                    // Allow a (useless) comma right after the closing parenthesis.
                    if self.token_tag(i) == T::Comma {
                        i += 1;
                    }
                    if self.token_tag(i) == T::Colon
                        && self.token_tag(i + 1) == T::Colon
                        && self.token_tag(i + 2) == T::StringLiteral
                    {
                        result.first_clobber = Some(i + 2);
                    }
                }
            },
        }

        result
    }

    fn full_while_components(&self, info: WhileComponents) -> While {
        let mut result = While {
            ast: WhileComponents { ..info },
            inline_token: None,
            label_token: None,
            payload_token: None,
            else_token: UNDEFINED_TOKEN,
            error_token: None,
        };

        let mut tok_i = info.while_token.saturating_sub(1);
        if self.token_tag(tok_i) == T::KeywordInline {
            result.inline_token = Some(tok_i);
            tok_i = tok_i.saturating_sub(1);
        }
        if self.token_tag(tok_i) == T::Colon
            && self.token_tag(tok_i.saturating_sub(1)) == T::Identifier
        {
            result.label_token = Some(tok_i - 1);
        }

        let last_cond_token = self.last_token(info.cond_expr);
        if self.token_tag(last_cond_token + 2) == T::Pipe {
            result.payload_token = Some(last_cond_token + 3);
        }

        if info.else_expr != 0 {
            // then_expr else |x|
            //           ^    ^
            result.else_token = self.last_token(info.then_expr) + 1;
            if self.token_tag(result.else_token + 1) == T::Pipe {
                result.error_token = Some(result.else_token + 2);
            }
        }

        result
    }

    fn full_for_components<'ast, 'buf>(&'ast self, info: ForComponents<'buf>) -> For<'buf>
    where
        'ast: 'buf,
    {
        let mut result = For {
            ast: ForComponents { ..info },
            inline_token: None,
            label_token: None,
            payload_token: UNDEFINED_TOKEN,
            else_token: UNDEFINED_TOKEN,
        };

        let mut tok_i = info.for_token.saturating_sub(1);
        if self.token_tag(tok_i) == T::KeywordInline {
            result.inline_token = Some(tok_i);
            tok_i = tok_i.saturating_sub(1);
        }
        if self.token_tag(tok_i) == T::Colon
            && self.token_tag(tok_i.saturating_sub(1)) == T::Identifier
        {
            result.label_token = Some(tok_i - 1);
        }

        let last_cond_token = self.last_token(info.inputs[info.inputs.len() - 1]);
        result.payload_token = match self.token_tag(last_cond_token + 1) {
            T::Comma => last_cond_token + 4,
            _ => last_cond_token + 3,
        };

        if info.else_expr != 0 {
            result.else_token = self.last_token(info.then_expr) + 1;
        }

        result
    }

    fn full_call_components<'ast, 'buf>(&'ast self, info: CallComponents<'buf>) -> Call<'buf>
    where
        'ast: 'buf,
    {
        let mut result = Call {
            ast: CallComponents { ..info },
            async_token: None,
        };

        let first_token = self.first_token(info.fn_expr);
        if first_token != 0 && self.token_tag(first_token - 1) == T::KeywordAsync {
            result.async_token = Some(first_token - 1);
        }

        result
    }

    pub fn full_var_decl(&self, node: node::Index) -> Option<VarDecl> {
        match self.node(node).tag {
            N::GlobalVarDecl => Some(self.global_var_decl(node)),
            N::LocalVarDecl => Some(self.local_var_decl(node)),
            N::AlignedVarDecl => Some(self.aligned_var_decl(node)),
            N::SimpleVarDecl => Some(self.simple_var_decl(node)),
            _ => None,
        }
    }

    pub fn full_if(&self, node: node::Index) -> Option<If> {
        match self.node(node).tag {
            N::IfSimple => Some(self.if_simple(node)),
            N::If => Some(self.if_full(node)),
            _ => None,
        }
    }

    pub fn full_while(&self, node: node::Index) -> Option<While> {
        match self.node(node).tag {
            N::WhileSimple => Some(self.while_simple(node)),
            N::WhileCont => Some(self.while_cont(node)),
            N::While => Some(self.while_full(node)),
            _ => None,
        }
    }

    pub fn full_for(&self, node: node::Index) -> Option<For> {
        match self.node(node).tag {
            N::ForSimple => Some(self.for_simple(node)),
            N::For => Some(self.for_full(node)),
            _ => None,
        }
    }

    pub fn full_container_field(&self, node: node::Index) -> Option<ContainerField> {
        match self.node(node).tag {
            N::ContainerFieldInit => Some(self.container_field_init(node)),
            N::ContainerFieldAlign => Some(self.container_field_align(node)),
            N::ContainerField => Some(self.container_field(node)),
            _ => None,
        }
    }

    pub fn full_fn_proto<'ast, 'buf>(
        &'ast self,
        buffer: &'buf mut [node::Index; 1],
        node: node::Index,
    ) -> Option<FnProto<'buf>>
    where
        'ast: 'buf,
    {
        match self.node(node).tag {
            N::FnProto => Some(self.fn_proto(node)),
            N::FnProtoMulti => Some(self.fn_proto_multi(node)),
            N::FnProtoOne => Some(self.fn_proto_one(buffer, node)),
            N::FnProtoSimple => Some(self.fn_proto_simple(buffer, node)),
            N::FnDecl => self.full_fn_proto(buffer, self.node(node).data.lhs),
            _ => None,
        }
    }

    pub fn full_struct_init<'ast, 'buf>(
        &'ast self,
        buffer: &'buf mut [node::Index; 2],
        node: node::Index,
    ) -> Option<StructInit<'buf>>
    where
        'ast: 'buf,
    {
        match self.node(node).tag {
            N::StructInitOne | N::StructInitOneComma => {
                let buffer = (&mut buffer[0..1]).try_into().unwrap();
                Some(self.struct_init_one(buffer, node))
            }
            N::StructInitDotTwo | N::StructInitDotTwoComma => {
                Some(self.struct_init_dot_two(buffer, node))
            }
            N::StructInitDot | N::StructInitDotComma => Some(self.struct_init_dot(node)),
            N::StructInit | N::StructInitComma => Some(self.struct_init(node)),
            _ => None,
        }
    }

    pub fn full_array_init<'ast, 'buf>(
        &'ast self,
        buffer: &'buf mut [node::Index; 2],
        node: node::Index,
    ) -> Option<ArrayInit<'buf>>
    where
        'ast: 'buf,
    {
        match self.node(node).tag {
            N::ArrayInitOne | N::ArrayInitOneComma => {
                let buffer = (&mut buffer[0..1]).try_into().unwrap();
                Some(self.array_init_one(buffer, node))
            }
            N::ArrayInitDotTwo | N::ArrayInitDotTwoComma => {
                Some(self.array_init_dot_two(buffer, node))
            }
            N::ArrayInitDot | N::ArrayInitDotComma => Some(self.array_init_dot(node)),
            N::ArrayInit | N::ArrayInitComma => Some(self.array_init(node)),
            _ => None,
        }
    }

    pub fn full_array_type(&self, node: node::Index) -> Option<ArrayType> {
        match self.node(node).tag {
            N::ArrayType => Some(self.array_type(node)),
            N::ArrayTypeSentinel => Some(self.array_type_sentinel(node)),
            _ => None,
        }
    }

    pub fn full_ptr_type(&self, node: node::Index) -> Option<PtrType> {
        match self.node(node).tag {
            N::PtrTypeAligned => Some(self.ptr_type_aligned(node)),
            N::PtrTypeSentinel => Some(self.ptr_type_sentinel(node)),
            N::PtrType => Some(self.ptr_type(node)),
            N::PtrTypeBitRange => Some(self.ptr_type_bit_range(node)),
            _ => None,
        }
    }

    pub fn full_slice(&self, node: node::Index) -> Option<Slice> {
        match self.node(node).tag {
            N::SliceOpen => Some(self.slice_open(node)),
            N::Slice => Some(self.slice(node)),
            N::SliceSentinel => Some(self.slice_sentinel(node)),
            _ => None,
        }
    }

    pub fn full_container_decl<'ast, 'buf>(
        &'ast self,
        buffer: &'buf mut [node::Index; 2],
        node: node::Index,
    ) -> Option<ContainerDecl<'buf>>
    where
        'ast: 'buf,
    {
        match self.node(node).tag {
            N::Root => Some(self.container_decl_root()),
            N::ContainerDecl | N::ContainerDeclTrailing => Some(self.container_decl(node)),
            N::ContainerDeclArg | N::ContainerDeclArgTrailing => {
                Some(self.container_decl_arg(node))
            }
            N::ContainerDeclTwo | N::ContainerDeclTwoTrailing => {
                Some(self.container_decl_two(buffer, node))
            }
            N::TaggedUnion | N::TaggedUnionTrailing => Some(self.tagged_union(node)),
            N::TaggedUnionEnumTag | N::TaggedUnionEnumTagTrailing => {
                Some(self.tagged_union_enum_tag(node))
            }
            N::TaggedUnionTwo | N::TaggedUnionTwoTrailing => {
                Some(self.tagged_union_two(buffer, node))
            }
            _ => None,
        }
    }

    pub fn full_switch_case(&self, node: node::Index) -> Option<SwitchCase> {
        match self.node(node).tag {
            N::SwitchCaseOne | N::SwitchCaseInlineOne => Some(self.switch_case_one(node)),
            N::SwitchCase | N::SwitchCaseInline => Some(self.switch_case(node)),
            _ => None,
        }
    }

    pub fn full_asm(&self, node: node::Index) -> Option<Asm> {
        match self.node(node).tag {
            N::AsmSimple => Some(self.asm_simple(node)),
            N::Asm => Some(self.asm_full(node)),
            _ => None,
        }
    }

    pub fn full_call<'ast, 'buf>(
        &'ast self,
        buffer: &'buf mut [node::Index; 1],
        node: node::Index,
    ) -> Option<Call<'buf>>
    where
        'ast: 'buf,
    {
        match self.node(node).tag {
            N::Call | N::CallComma | N::AsyncCall | N::AsyncCallComma => Some(self.call_full(node)),
            N::CallOne | N::CallOneComma | N::AsyncCallOne | N::AsyncCallOneComma => {
                Some(self.call_one(buffer, node))
            }
            _ => None,
        }
    }
}

pub struct VarDecl {
    visib_token: Option<TokenIndex>,
    extern_export_token: Option<TokenIndex>,
    lib_name: Option<TokenIndex>,
    threadlocal_token: Option<TokenIndex>,
    comptime_token: Option<TokenIndex>,
    ast: VarDeclComponents,
}

pub struct VarDeclComponents {
    mut_token: TokenIndex,
    type_node: node::Index,
    align_node: node::Index,
    addrspace_node: node::Index,
    section_node: node::Index,
    init_node: node::Index,
}

impl VarDecl {
    pub fn first_token(&self) -> TokenIndex {
        return self
            .visib_token
            .or(self.extern_export_token)
            .or(self.threadlocal_token)
            .or(self.comptime_token)
            .unwrap_or(self.ast.mut_token);
    }
}

pub struct If {
    /// Points to the first token after the `|`. Will either be an identifier or
    /// a `*` (with an identifier immediately after it).
    payload_token: Option<TokenIndex>,
    /// Points to the identifier after the `|`.
    error_token: Option<TokenIndex>,
    /// Populated only if else_expr != 0.
    else_token: TokenIndex,
    ast: IfComponents,
}

pub struct IfComponents {
    if_token: TokenIndex,
    cond_expr: node::Index,
    then_expr: node::Index,
    else_expr: node::Index,
}

pub struct While {
    ast: WhileComponents,
    inline_token: Option<TokenIndex>,
    label_token: Option<TokenIndex>,
    payload_token: Option<TokenIndex>,
    error_token: Option<TokenIndex>,
    /// Populated only if else_expr != 0.
    else_token: TokenIndex,
}

pub struct WhileComponents {
    while_token: TokenIndex,
    cond_expr: node::Index,
    cont_expr: node::Index,
    then_expr: node::Index,
    else_expr: node::Index,
}

pub struct For<'buf> {
    ast: ForComponents<'buf>,
    inline_token: Option<TokenIndex>,
    label_token: Option<TokenIndex>,
    payload_token: TokenIndex,
    /// Populated only if else_expr != 0.
    else_token: TokenIndex,
}

pub struct ForComponents<'buf> {
    for_token: TokenIndex,
    inputs: &'buf [node::Index],
    then_expr: node::Index,
    else_expr: node::Index,
}

pub struct ContainerField {
    comptime_token: Option<TokenIndex>,
    ast: ContainerFieldComponents,
}

pub struct ContainerFieldComponents {
    main_token: TokenIndex,
    type_expr: node::Index,
    align_expr: node::Index,
    value_expr: node::Index,
    tuple_like: bool,
}

impl ContainerField {
    pub fn first_token(&self) -> TokenIndex {
        self.comptime_token.unwrap_or(self.ast.main_token)
    }

    pub fn convert_to_non_tuple_like(&mut self, tree: &Ast) {
        if !self.ast.tuple_like {
            return;
        }
        if self.ast.type_expr == 0 {
            return;
        }
        if tree.node(self.ast.type_expr).tag != N::Identifier {
            return;
        }

        let ident = tree.node(self.ast.type_expr).main_token;
        self.ast.tuple_like = false;
        self.ast.main_token = ident;
        self.ast.type_expr = 0;
    }
}

pub struct FnProto<'buf> {
    visib_token: Option<TokenIndex>,
    extern_export_inline_token: Option<TokenIndex>,
    lib_name: Option<TokenIndex>,
    name_token: Option<TokenIndex>,
    lparen: TokenIndex,
    ast: FnProtoComponents<'buf>,
}

pub struct FnProtoComponents<'buf> {
    proto_node: node::Index,
    fn_token: TokenIndex,
    return_type: node::Index,
    params: &'buf [node::Index],
    align_expr: node::Index,
    addrspace_expr: node::Index,
    section_expr: node::Index,
    callconv_expr: node::Index,
}

pub struct FnParam {
    first_doc_comment: Option<TokenIndex>,
    name_token: Option<TokenIndex>,
    comptime_noalias: Option<TokenIndex>,
    anytype_ellipsis3: Option<TokenIndex>,
    type_expr: node::Index,
}

pub struct FnParamIterator<'ast, 'src, 'fun, 'buf> {
    tree: &'ast Ast<'src>,
    fn_proto: &'fun FnProto<'buf>,
    param_i: usize,
    tok_i: TokenIndex,
    tok_flag: bool,
}

impl Iterator for FnParamIterator<'_, '_, '_, '_> {
    type Item = FnParam;

    fn next(&mut self) -> Option<FnParam> {
        loop {
            let mut first_doc_comment: Option<TokenIndex> = None;
            let mut comptime_noalias: Option<TokenIndex> = None;
            let mut name_token: Option<TokenIndex> = None;
            if !self.tok_flag {
                if self.param_i >= self.fn_proto.ast.params.len() {
                    return None;
                }
                let param_type = self.fn_proto.ast.params[self.param_i];
                let mut tok_i = self.tree.first_token(param_type) - 1;
                loop {
                    match self.tree.token_tag(tok_i) {
                        T::Colon => {}
                        T::Identifier => name_token = Some(tok_i),
                        T::DocComment => first_doc_comment = Some(tok_i),
                        T::KeywordComptime | T::KeywordNoalias => comptime_noalias = Some(tok_i),
                        _ => break,
                    }
                    tok_i -= 1;
                }
                self.param_i += 1;
                self.tok_i = self.tree.last_token(param_type) + 1;
                if self.tree.token_tag(self.tok_i) == T::Comma {
                    self.tok_i += 1;
                }
                self.tok_flag = true;
                return Some(FnParam {
                    first_doc_comment,
                    comptime_noalias,
                    name_token,
                    anytype_ellipsis3: None,
                    type_expr: param_type,
                });
            }
            if self.tree.token_tag(self.tok_i) == T::Comma {
                self.tok_i += 1;
            }
            if self.tree.token_tag(self.tok_i) == T::RParen {
                return None;
            }
            if self.tree.token_tag(self.tok_i) == T::DocComment {
                first_doc_comment = Some(self.tok_i);
                while self.tree.token_tag(self.tok_i) == T::DocComment {
                    self.tok_i += 1;
                }
            }
            match self.tree.token_tag(self.tok_i) {
                T::Ellipsis3 => {
                    self.tok_flag = false; // Next iteration should return None.
                    return Some(FnParam {
                        first_doc_comment,
                        comptime_noalias: None,
                        name_token: None,
                        anytype_ellipsis3: Some(self.tok_i),
                        type_expr: 0,
                    });
                }
                T::KeywordNoalias | T::KeywordComptime => {
                    comptime_noalias = Some(self.tok_i);
                    self.tok_i += 1;
                }
                _ => {}
            }
            if self.tree.token_tag(self.tok_i) == T::Identifier
                && self.tree.token_tag(self.tok_i + 1) == T::Colon
            {
                name_token = Some(self.tok_i);
                self.tok_i += 2;
            }
            if self.tree.token_tag(self.tok_i) == T::KeywordAnytype {
                self.tok_i += 1;
                return Some(FnParam {
                    first_doc_comment,
                    comptime_noalias,
                    name_token,
                    anytype_ellipsis3: Some(self.tok_i - 1),
                    type_expr: 0,
                });
            }
            self.tok_flag = false;
        }
    }
}

impl<'buf> FnProto<'buf> {
    pub fn first_token(&self) -> TokenIndex {
        self.visib_token
            .or(self.extern_export_inline_token)
            .unwrap_or(self.ast.fn_token)
    }

    pub fn iterate<'fun, 'ast, 'src>(
        &'fun self,
        tree: &'ast Ast<'src>,
    ) -> FnParamIterator<'ast, 'src, 'fun, 'buf> {
        FnParamIterator {
            tree,
            fn_proto: self,
            param_i: 0,
            tok_i: self.lparen + 1,
            tok_flag: true,
        }
    }
}

pub struct StructInit<'buf> {
    ast: StructInitComponents<'buf>,
}

pub struct StructInitComponents<'buf> {
    lbrace: TokenIndex,
    fields: &'buf [node::Index],
    type_expr: node::Index,
}

pub struct ArrayInit<'buf> {
    ast: ArrayInitComponents<'buf>,
}

pub struct ArrayInitComponents<'buf> {
    lbrace: TokenIndex,
    elements: &'buf [node::Index],
    type_expr: node::Index,
}

pub struct ArrayType {
    ast: ArrayTypeComponents,
}

pub struct ArrayTypeComponents {
    lbracket: TokenIndex,
    elem_count: node::Index,
    sentinel: node::Index,
    elem_type: node::Index,
}

pub enum PtrSize {
    One,
    Many,
    Slice,
    C,
}

pub struct PtrType {
    size: PtrSize,
    allowzero_token: Option<TokenIndex>,
    const_token: Option<TokenIndex>,
    volatile_token: Option<TokenIndex>,
    ast: PtrTypeComponents,
}

pub struct PtrTypeComponents {
    main_token: TokenIndex,
    align_node: node::Index,
    addrspace_node: node::Index,
    sentinel: node::Index,
    bit_range_start: node::Index,
    bit_range_end: node::Index,
    child_type: node::Index,
}

pub struct Slice {
    ast: SliceComponents,
}

pub struct SliceComponents {
    sliced: node::Index,
    lbracket: TokenIndex,
    start: node::Index,
    end: node::Index,
    sentinel: node::Index,
}

pub struct ContainerDecl<'buf> {
    layout_token: Option<TokenIndex>,
    ast: ContainerDeclComponents<'buf>,
}

pub struct ContainerDeclComponents<'buf> {
    main_token: TokenIndex,
    /// Populated when main_token is Keyword_union.
    enum_token: Option<TokenIndex>,
    members: &'buf [node::Index],
    arg: node::Index,
}

pub struct SwitchCase<'buf> {
    inline_token: Option<TokenIndex>,
    /// Points to the first token after the `|`. Will either be an identifier or
    /// a `*` (with an identifier immediately after it).
    payload_token: Option<TokenIndex>,
    ast: SwitchCaseComponents<'buf>,
}

pub struct SwitchCaseComponents<'buf> {
    /// If empty, this is an else case
    values: &'buf [node::Index],
    arrow_token: TokenIndex,
    target_expr: node::Index,
}

pub struct Asm<'buf> {
    ast: AsmComponents<'buf>,
    volatile_token: Option<TokenIndex>,
    first_clobber: Option<TokenIndex>,
    outputs: &'buf [node::Index],
    inputs: &'buf [node::Index],
}

pub struct AsmComponents<'buf> {
    asm_token: TokenIndex,
    template: node::Index,
    items: &'buf [node::Index],
    rparen: TokenIndex,
}

pub struct Call<'buf> {
    ast: CallComponents<'buf>,
    async_token: Option<TokenIndex>,
}

pub struct CallComponents<'buf> {
    lparen: TokenIndex,
    fn_expr: node::Index,
    params: &'buf [node::Index],
}
