// Copyright (c) 2025 YiZhonghua<zhyi@dpai.com>. All rights reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
#pragma once

#include "toka/SourceLocation.h"
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

namespace toka {
class SourceManager;

struct DiagLoc {
  std::string File;
  int Line;
  int Col;
};

enum class DiagLevel { Warning, Error, Note };

enum class DiagID {
#define DIAG(ID, Level, Code, Msg) ID,
#include "toka/DiagnosticDefs.def"
#undef DIAG
  NUM_DIAGNOSTICS
};

class DiagnosticEngine {
public:
  static int ErrorCount;
  static SourceManager *SrcMgr;

  static void init(SourceManager &SM) { SrcMgr = &SM; }

  static bool hasErrors() { return ErrorCount > 0; }

  // Variadic template for handling arguments
  template <typename... Args>
  static void report(DiagLoc loc, DiagID id, Args &&...args) {
    reportImpl(loc, id, formatMessage(id, std::forward<Args>(args)...));
  }

  template <typename... Args>
  static void report(SourceLocation loc, DiagID id, Args &&...args) {
    reportImpl(loc, id, formatMessage(id, std::forward<Args>(args)...));
  }

private:
  static void reportImpl(DiagLoc loc, DiagID id, const std::string &message);
  static void reportImpl(SourceLocation loc, DiagID id,
                         const std::string &message);
  static const char *getFormatString(DiagID id);
  static DiagLevel getLevel(DiagID id);
  static const char *getCode(DiagID id);

  // Poor Man's Format:

  // Template for arithmetic types (to avoid ambiguity with strings)
  template <typename T,
            typename std::enable_if<
                std::is_arithmetic<typename std::decay<T>::type>::value,
                int>::type = 0>
  static std::string toString(T &&val) {
    return std::to_string(std::forward<T>(val));
  }

  // Overload for strings
  static std::string toString(const std::string &val) { return val; }
  static std::string toString(const char *val) {
    return std::string(val != nullptr ? val : "(null)");
  }
  static std::string toString(char *val) {
    return std::string(val != nullptr ? val : "(null)");
  }

  // Recursive helper to format message
  template <typename T, typename... Args>
  static void formatHelper(std::string &fmt, size_t pos, T &&arg,
                           Args &&...args) {
    size_t placeholder = fmt.find("{}", pos);
    if (placeholder != std::string::npos) {
      std::string val = toString(std::forward<T>(arg));
      fmt.replace(placeholder, 2, val);
      formatHelper(fmt, placeholder + val.length(),
                   std::forward<Args>(args)...);
    }
  }

  static void formatHelper(std::string &fmt, size_t pos) {
    // Base case: no more args
  }

  template <typename... Args>
  static std::string formatMessage(DiagID id, Args &&...args) {
    std::string fmt = getFormatString(id);
    formatHelper(fmt, 0, std::forward<Args>(args)...);
    return fmt;
  }
};

} // namespace toka
