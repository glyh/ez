#include "scoped_environment.hpp"
#include <cstdio>
#include <string>

scoped_environment::scoped_environment() : scopes_stack({}) {}

AllocaInst *scoped_environment::find(const std::string &name) {
  AllocaInst *result = nullptr;
  for (auto it = scopes_stack.rbegin(); it != scopes_stack.rend(); it++) {
    auto scope = *it;
    auto alloc = scope.find(name);
    if (alloc != scope.end()) {
      return alloc->second;
    }
  }
  return nullptr;
}

void scoped_environment::add(const std::string &name, AllocaInst *alloc) {
  std::unordered_map<std::string, AllocaInst *> &last = scopes_stack.back();
  last.insert({name, alloc});
}

void scoped_environment::enter_scope() { scopes_stack.push_back({}); }

void scoped_environment::exit_scope() { scopes_stack.pop_back(); }
