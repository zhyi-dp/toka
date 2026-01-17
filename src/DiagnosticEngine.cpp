// src/DiagnosticEngine.cpp
#include "toka/DiagnosticEngine.h"
#include <iostream>

namespace toka {

int DiagnosticEngine::ErrorCount = 0;

const char *DiagnosticEngine::getFormatString(DiagID id) {
  switch (id) {
#define DIAG(ID, Level, Msg)                                                   \
  case DiagID::ID:                                                             \
    return Msg;
#include "toka/DiagnosticDefs.def"
#undef DIAG
  case DiagID::NUM_DIAGNOSTICS:
    return "Unknown Error";
  default:
    return "Unknown Error";
  }
}

DiagLevel DiagnosticEngine::getLevel(DiagID id) {
  switch (id) {
#define DIAG(ID, Level, Msg)                                                   \
  case DiagID::ID:                                                             \
    return DiagLevel::Level;
#include "toka/DiagnosticDefs.def"
#undef DIAG
  case DiagID::NUM_DIAGNOSTICS:
    return DiagLevel::Error;
  default:
    return DiagLevel::Error;
  }
}

void DiagnosticEngine::reportImpl(SourceLocation loc, DiagID id,
                                  const std::string &message) {
  DiagLevel level = getLevel(id);

  // Update stats
  if (level == DiagLevel::Error) {
    ErrorCount++;
  }

  // Color codes
  const char *color = "";
  const char *reset = "\033[0m";
  const char *levelStr = "";

  if (level == DiagLevel::Error) {
    color = "\033[1;31m"; // Red Bold
    levelStr = "error";
  } else if (level == DiagLevel::Warning) {
    color = "\033[1;33m"; // Yellow Bold
    levelStr = "warning";
  } else {
    color = "\033[1;36m"; // Cyan Bold
    levelStr = "note";
  }

  std::cerr << loc.File << ":" << loc.Line << ":" << loc.Col << ": " << color
            << levelStr << ": " << reset << message << "\n";

  // Kill Switch
  if (ErrorCount > 20) {
    std::cerr
        << "\033[1;31mfatal:\033[0m too many errors emitted, stopping now.\n";
    exit(1);
  }
}

} // namespace toka
