#pragma once

#include "ez_ir.pb.h"

std::string ez_binop_to_string(ez_proto::Expr_Binary_BinOp);
std::string ez_type_to_string(const ez_proto::EzType &);
