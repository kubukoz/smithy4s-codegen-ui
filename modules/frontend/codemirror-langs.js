import { StreamLanguage } from "@codemirror/language";
import { scala } from "@codemirror/legacy-modes/mode/clike";
import { githubLight } from "@uiw/codemirror-theme-github";
import { autocompletion, completeAnyWord } from "@codemirror/autocomplete";

// Minimal Smithy tokenizer as a CodeMirror 5-style stream language
const smithyMode = {
  name: "smithy",
  languageData: {
    commentTokens: { line: "//" },
  },
  startState: () => ({ inBlockComment: false, inDocComment: false }),
  token(stream, state) {
    // Block doc comments: /// or ///
    if (state.inDocComment) {
      if (stream.match(/.*?\*\//)) { state.inDocComment = false; }
      else { stream.skipToEnd(); }
      return "comment";
    }
    if (state.inBlockComment) {
      if (stream.match(/.*?\*\//)) { state.inBlockComment = false; }
      else { stream.skipToEnd(); }
      return "comment";
    }

    // Whitespace
    if (stream.eatSpace()) return null;

    // Doc comment ///
    if (stream.match("///")) {
      stream.skipToEnd();
      return "comment";
    }
    // Line comment //
    if (stream.match("//")) {
      stream.skipToEnd();
      return "comment";
    }
    // Block comment /*
    if (stream.match("/*")) {
      state.inBlockComment = true;
      return "comment";
    }

    // Strings
    if (stream.match('"""')) {
      while (!stream.match('"""')) {
        if (!stream.next()) break;
      }
      return "string";
    }
    if (stream.match('"')) {
      while (true) {
        const ch = stream.next();
        if (ch === '"' || ch == null) break;
        if (ch === '\\') stream.next();
      }
      return "string";
    }

    // Numbers
    if (stream.match(/^-?[0-9]+(\.[0-9]+)?/)) return "number";

    // Annotations (@trait)
    if (stream.match(/^@[A-Za-z_][A-Za-z0-9_#.]*/)) return "meta";

    // Keywords
    if (stream.match(/^(namespace|use|service|operation|structure|union|list|map|set|enum|intEnum|blob|boolean|string|byte|short|integer|long|float|double|bigInteger|bigDecimal|timestamp|document|resource|trait|apply|with|for|from|metadata|version)\b/)) {
      return "keyword";
    }

    // Control keywords
    if (stream.match(/^(input|output|errors|mixins)\b/)) return "keyword";

    // Identifiers
    if (stream.match(/^[A-Za-z_][A-Za-z0-9_]*/)) return "variableName";

    // Punctuation / operators
    if (stream.match(/^[{}[\]:,=|$#]/)) return "punctuation";

    stream.next();
    return null;
  },
};

export const smithyLanguage = StreamLanguage.define(smithyMode);
export const scalaLanguage = StreamLanguage.define(scala);
export { githubLight };

export const smithyCompletion = autocompletion({ override: [completeAnyWord] });
