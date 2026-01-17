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
#include "toka/SourceManager.h"
#include <algorithm>
#include <fstream>
#include <iostream>
#include <sstream>

namespace toka {

SourceManager::FileInfo::FileInfo(std::string Name, std::string Data,
                                  uint32_t Start)
    : FileName(std::move(Name)), Content(std::move(Data)),
      GlobalStartOffset(Start), GlobalEndOffset(Start + Content.size()) {
  calculateLineOffsets();
}

void SourceManager::FileInfo::calculateLineOffsets() {
  LineStartOffsets.clear();
  LineStartOffsets.push_back(0); // Line 1 starts at 0

  for (size_t i = 0; i < Content.size(); ++i) {
    if (Content[i] == '\n') {
      LineStartOffsets.push_back(i + 1);
    }
  }
}

SourceLocation SourceManager::loadFile(const std::string &Path) {
  // Check if already loaded
  for (const auto &File : Files) {
    if (File.FileName == Path) {
      return SourceLocation(File.GlobalStartOffset);
    }
  }

  std::ifstream Input(Path);
  if (!Input) {
    std::cerr << "Error: Could not open file " << Path << "\n";
    // Return empty content on failure? Or preserve existing behavior (exit)?
    // For now, let's create an empty file entry to avoid crashes in locating
    return addFile(Path, "");
  }

  std::stringstream Buffer;
  Buffer << Input.rdbuf();
  return addFile(Path, Buffer.str());
}

SourceLocation SourceManager::addFile(const std::string &Path,
                                      std::string Content) {
  // Check if already loaded to avoid duplicates?
  // We allow re-adding for "addFile" if it's explicitly called, but usually
  // only loadFile does checking. Actually, let's check duplicates for safety to
  // keep offsets unique per path.
  for (const auto &File : Files) {
    if (File.FileName == Path) {
      // Re-loading same path? We can't overwrite easily in global space.
      // Return existing.
      return SourceLocation(File.GlobalStartOffset);
    }
  }

  uint32_t Start = NextOffset;
  Files.emplace_back(Path, std::move(Content), Start);
  NextOffset = Files.back().GlobalEndOffset +
               1; // +1 to leave a gap between files? Or just adjacent.
  // Let's keep them adjacent.
  // Actually, FileInfo uses EndOffset = Start + Size.
  // So NextOffset can be exactly EndOffset.
  NextOffset = Files.back().GlobalEndOffset;

  return SourceLocation(Start);
}

FullSourceLoc SourceManager::getFullSourceLoc(SourceLocation Loc) const {
  if (Loc.isInvalid())
    return FullSourceLoc();

  uint32_t Offset = Loc.getRawEncoding();

  // Find the file containing this offset
  auto It = std::upper_bound(Files.begin(), Files.end(), Offset,
                             [](uint32_t Off, const FileInfo &FI) {
                               return Off < FI.GlobalEndOffset;
                             });

  if (It == Files.end()) {
    // Should not happen if Loc is valid and within range
    return FullSourceLoc();
  }

  // It points to the first file where Offset < GlobalEndOffset.
  // We need to check if Offset >= GlobalStartOffset.
  if (Offset < It->GlobalStartOffset) {
    // Should not happen with valid Locs (gaps?)
    return FullSourceLoc();
  }

  const FileInfo &File = *It;
  uint32_t FileRelativeOffset = Offset - File.GlobalStartOffset;

  // Find line number
  auto LineIt =
      std::upper_bound(File.LineStartOffsets.begin(),
                       File.LineStartOffsets.end(), FileRelativeOffset);

  // LineIt points to the first line start > our offset.
  // So the line index is (LineIt - begin) - 1.
  // But Lines are 1-based, so just (LineIt - begin).
  unsigned Line = std::distance(File.LineStartOffsets.begin(), LineIt);

  // The start of that line
  uint32_t LineStart = File.LineStartOffsets[Line - 1];

  // Column is 1-based
  unsigned Col = FileRelativeOffset - LineStart + 1;

  return FullSourceLoc{File.FileName.c_str(), Line, Col};
}

std::string_view SourceManager::getBufferData(SourceLocation Loc) const {
  if (Loc.isInvalid())
    return "";

  uint32_t Offset = Loc.getRawEncoding();
  auto It = std::upper_bound(Files.begin(), Files.end(), Offset,
                             [](uint32_t Off, const FileInfo &FI) {
                               return Off < FI.GlobalEndOffset;
                             });

  if (It == Files.end() || Offset < It->GlobalStartOffset)
    return "";

  return It->Content;
}

} // namespace toka
