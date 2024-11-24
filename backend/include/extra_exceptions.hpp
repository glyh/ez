#pragma once

#include <format>
#include <stdexcept>

#include "ez_ir.pb.h"
#include "pcap-bpf.h"

#include "ezdebug.hpp"

class Unreachable : public std::logic_error {
public:
  Unreachable(const std::string &name) : std::logic_error(name) {};
};

class NotImplemented : public std::logic_error {
public:
  NotImplemented(const char *reason) : std::logic_error(reason) {};
  NotImplemented(std::string reason) : std::logic_error(reason) {};
};

class UndefinedVariable : public std::logic_error {
public:
  UndefinedVariable(const std::string &name) : std::logic_error(name) {};
};

class BinOpMismatch : public std::logic_error {
public:
  BinOpMismatch(ez_proto::Expr_Binary_BinOp op, const ez_proto::EzType &ty)
      : std::logic_error(std::format("Can't apply {} on {}", "",
                                     ez_binop_to_string(op),
                                     ez_type_to_string(ty))) {};
};

class InputEror : public std::logic_error {
public:
  InputEror(const std::string &file_name) : std::logic_error(file_name) {};
};
