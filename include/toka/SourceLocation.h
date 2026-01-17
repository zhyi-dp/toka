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

#include <cstdint>
#include <functional>

namespace toka {

/// SourceLocation - This is a carefully crafted 32-bit value that encodes
/// a location in the translation unit source code.
class SourceLocation {
  uint32_t ID = 0;

public:
  SourceLocation() = default;
  explicit SourceLocation(uint32_t ID) : ID(ID) {}

  bool isValid() const { return ID != 0; }
  bool isInvalid() const { return ID == 0; }

  uint32_t getRawEncoding() const { return ID; }
  static SourceLocation getFromRawEncoding(uint32_t Encoding) {
    return SourceLocation(Encoding);
  }

  bool operator==(const SourceLocation &RHS) const { return ID == RHS.ID; }
  bool operator!=(const SourceLocation &RHS) const { return ID != RHS.ID; }
  bool operator<(const SourceLocation &RHS) const { return ID < RHS.ID; }
};

/// FullSourceLoc - This represents a resolved source location, containing
/// filename, line, and column information.
struct FullSourceLoc {
  const char *FileName = "";
  unsigned Line = 0;
  unsigned Column = 0;

  bool isValid() const { return FileName && *FileName; }
};

} // namespace toka

namespace std {
template <> struct hash<toka::SourceLocation> {
  size_t operator()(const toka::SourceLocation &Loc) const {
    return Loc.getRawEncoding();
  }
};
} // namespace std
