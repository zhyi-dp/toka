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
#include "toka/DiagnosticEngine.h"
#include "toka/SourceManager.h"
#include <iostream>

namespace toka {

SourceManager *DiagnosticEngine::SrcMgr = nullptr;
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

void DiagnosticEngine::reportImpl(DiagLoc loc, DiagID id,
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

void DiagnosticEngine::reportImpl(SourceLocation loc, DiagID id,
                                  const std::string &message) {
  if (SrcMgr) {
    FullSourceLoc Full = SrcMgr->getFullSourceLoc(loc);
    DiagLoc DL{Full.FileName, (int)Full.Line, (int)Full.Column};
    reportImpl(DL, id, message);
  } else {
    DiagLoc DL{"<unknown>", 0, 0};
    reportImpl(DL, id,
               message + " [RawLoc: " + std::to_string(loc.getRawEncoding()) +
                   "]");
  }
}

} // namespace toka
