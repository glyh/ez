#pragma once

#include <format>
#include <stdexcept>

#include "ez_ir.pb.h"
#include "pcap-bpf.h"

#include "alias.hpp"
#include "ezdebug.hpp"

class Unreachable : public std::logic_error {
public:
  Unreachable(const str &name) : std::logic_error(name) {};
};

class NotImplemented : public std::logic_error {
public:
  NotImplemented(const char *reason) : std::logic_error(reason) {};
  NotImplemented(str reason) : std::logic_error(reason) {};
};

class UndefinedVariable : public std::logic_error {
public:
  UndefinedVariable(const str &name) : std::logic_error(name) {};
};

class BinOpMismatch : public std::logic_error {
public:
  BinOpMismatch(ez_proto::Expr_Binary_BinOp op, const ez_proto::EzType &ty)
      : std::logic_error(std::format("Can't apply {} on {}", "",
                                     ez_binop_to_string(op),
                                     ez_type_to_string(ty))) {};
};
