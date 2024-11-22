#include "ezdebug.hpp"
#include "ez_ir.pb.h"
#include <format>
#include <stdexcept>

str ez_binop_to_string(ez_proto::Expr_Binary_BinOp bop) {
  switch (bop) {
  case ez_proto::Expr_Binary_BinOp_ADD:
    return "+";
  case ez_proto::Expr_Binary_BinOp_SUB:
    return "-";
  case ez_proto::Expr_Binary_BinOp_MUL:
    return "*";
  case ez_proto::Expr_Binary_BinOp_DIV:
    return "/";
  case ez_proto::Expr_Binary_BinOp_MOD:
    return "%";
  case ez_proto::Expr_Binary_BinOp_EQ:
    return "==";
  case ez_proto::Expr_Binary_BinOp_NOTEQ:
    return "!=";
  case ez_proto::Expr_Binary_BinOp_LESSTHAN:
    return "<";
  case ez_proto::Expr_Binary_BinOp_LESSEQ:
    return "<=";
  case ez_proto::Expr_Binary_BinOp_GREATERTHAN:
    return ">";
  case ez_proto::Expr_Binary_BinOp_GREATEREQ:
    return ">=";
  case ez_proto::Expr_Binary_BinOp_LOR:
    return "or";
  case ez_proto::Expr_Binary_BinOp_LAND:
    return "and";
  default:
    throw std::domain_error("invalid bin op");
  }
}

str ez_type_to_string(const ez_proto::EzType &ty) {
  switch (ty.kind_case()) {
  case ez_proto::EzType::kNonPtr:
    switch (ty.non_ptr()) {
    case ez_proto::EzType_NonPtr_UNIT:
      return "unit";
    case ez_proto::EzType_NonPtr_I64:
      return "i64";
    case ez_proto::EzType_NonPtr_F64:
      return "f64";
    case ez_proto::EzType_NonPtr_BOOL:
      return "bool";
    case ez_proto::EzType_NonPtr_STR:
      return "string";
    }
  case ez_proto::EzType::kPtr:
    return std::format("ptr[{}]", ez_type_to_string(ty.ptr()));
  default:
    throw std::domain_error("invalid ez type");
  }
}
