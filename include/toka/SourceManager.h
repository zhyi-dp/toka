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
#include <string_view>
#include <vector>

namespace toka {

class SourceManager {
  struct FileInfo {
    std::string FileName;
    std::string Content;
    uint32_t GlobalStartOffset;
    uint32_t GlobalEndOffset;

    // Offsets within Content where each line starts.
    // Line 1 is always at index 0.
    std::vector<uint32_t> LineStartOffsets;

    FileInfo(std::string Name, std::string Data, uint32_t Start);

    void calculateLineOffsets();
  };

  std::vector<FileInfo> Files;
  uint32_t NextOffset = 1; // 0 is invalid

public:
  SourceManager() = default;
  SourceManager(const SourceManager &) = delete;
  SourceManager &operator=(const SourceManager &) = delete;

  /// Load a file from disk. Returns the SourceLocation pointing to the
  /// beginning of the file content in the virtual location space.
  SourceLocation loadFile(const std::string &Path);

  /// Add a file from memory content. Useful for testing or REPL.
  SourceLocation addFile(const std::string &Path, std::string Content);

  /// Return true if the location is valid.
  bool isValid(SourceLocation Loc) const { return Loc.isValid(); }

  /// Resolve a SourceLocation to its full file/line/column info.
  FullSourceLoc getFullSourceLoc(SourceLocation Loc) const;

  /// Get the raw buffer content for the file containing Loc.
  std::string_view getBufferData(SourceLocation Loc) const;
};

} // namespace toka
