#pragma once

#include "ez_ir.pb.h"

#include "alias.hpp"

str ez_binop_to_string(ez_proto::Expr_Binary_BinOp);
str ez_type_to_string(const ez_proto::EzType &);
