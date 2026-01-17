// include/toka/DiagnosticEngine.h
#pragma once

#include <string>
#include <type_traits>
#include <utility>
#include <vector>

namespace toka {

struct SourceLocation {
  std::string File;
  int Line;
  int Col;
};

enum class DiagLevel { Warning, Error, Note };

enum class DiagID {
#define DIAG(ID, Level, Msg) ID,
#include "toka/DiagnosticDefs.def"
#undef DIAG
  NUM_DIAGNOSTICS
};

class DiagnosticEngine {
public:
  static int ErrorCount;

  static bool hasErrors() { return ErrorCount > 0; }

  // Variadic template for handling arguments
  template <typename... Args>
  static void report(SourceLocation loc, DiagID id, Args &&...args) {
    reportImpl(loc, id, formatMessage(id, std::forward<Args>(args)...));
  }

private:
  static void reportImpl(SourceLocation loc, DiagID id,
                         const std::string &message);
  static const char *getFormatString(DiagID id);
  static DiagLevel getLevel(DiagID id);

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
