#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <string>
#include <vector>
#include <cctype>
#include <cstring>   
#include <utility>

using namespace std;

enum class TokenType {
    Identifier,
    Keyword,
    IntegerLiteral,
    FloatLiteral,
    StringLiteral,
    CharLiteral,
    Operator,
    Punctuation,
    Preprocessor,
    Comment,
    EndOfFile,
    Unknown
};

struct Token {
    TokenType type;
    string lexeme;
    int line;
    int column;
};

static string tokenTypeName(TokenType t) {
    switch (t) {
    case TokenType::Identifier: return "Identifier";
    case TokenType::Keyword: return "Keyword";
    case TokenType::IntegerLiteral: return "IntegerLiteral";
    case TokenType::FloatLiteral: return "FloatLiteral";
    case TokenType::StringLiteral: return "StringLiteral";
    case TokenType::CharLiteral: return "CharLiteral";
    case TokenType::Operator: return "Operator";
    case TokenType::Punctuation: return "Punctuation";
    case TokenType::Preprocessor: return "Preprocessor";
    case TokenType::Comment: return "Comment";
    case TokenType::EndOfFile: return "End Of File";
    default: return "Unknown";
    }
}

struct BorNode {
    BorNode* next[256] = { nullptr };
    bool terminal = false;
    string value;
};

class Bor {
public:
    Bor() { root = new BorNode(); }
    ~Bor() { clear(root); }

    void insert(const string& s) {
        BorNode* cur = root;
        for (unsigned char c : s) {
            if (!cur->next[c]) cur->next[c] = new BorNode();
            cur = cur->next[c];
        }
        cur->terminal = true;
        cur->value = s;
    }

    pair<int, string> longestMatch(const string& s, int pos) const {
        BorNode* cur = root;
        int bestLen = 0;
        string bestVal;
        int i = pos;
        while (i < s.size()) {
            unsigned char c = s[i];
            if (!cur->next[c]) break;
            cur = cur->next[c];
            ++i;
            if (cur->terminal) {
                bestLen = i - pos;
                bestVal = cur->value;
            }
        }
        return { bestLen, bestVal };
    }

private:
    BorNode* root;
    void clear(BorNode* node) {
        if (!node) return;
        for (int i = 0; i < 256; ++i) {
            if (node->next[i]) clear(node->next[i]);
        }
        delete node;
    }
};

class Lexer {
public:
    Lexer(const string& src) : src(src), pos(0), line(1), col(1) {
        buildKeywordBor();
        buildOperatorBor();
    }

    vector<Token> tokenize() {
        vector<Token> tokens;
        while (true) {
            skipWhitespace();
            if (eof()) {
                tokens.push_back({TokenType::EndOfFile, "", line, col});
                break;
            }
            int startLine = line, startCol = col;
            char c = curChar();
            if (c == '#' && (col == 1)) {
                string pp = readUntil('\n');
                tokens.push_back({TokenType::Preprocessor, pp, startLine, startCol});
                continue;
            }
            // комментарии
            if (c == '/') {
                if (curChar(1) == '/') {
                    string com = readUntil('\n');
                    tokens.push_back({TokenType::Comment, com, startLine, startCol});
                    continue;
                }
                else if (curChar(1) == '*') {
                    string com = readBlockComment();
                    tokens.push_back({TokenType::Comment, com, startLine, startCol});
                    continue;
                }
            }
            // строковый литерал
            if (c == '"') {
                string s = readStringLiteral();
                tokens.push_back({TokenType::StringLiteral, s, startLine, startCol});
                continue;
            }
            // символ литерал
            if (c == '\'') {
                string s = readCharLiteral();
                tokens.push_back({TokenType::CharLiteral, s, startLine, startCol});
                continue;
            }
            // число
            if (isdigit(c) || (c == '.' && isdigit(curChar(1)))) {
                Token t = readNumber();
                t.line = startLine; t.column = startCol;
                tokens.push_back(t);
                continue;
            }
            // идентификаторы; зарезервированные слова
            if (isalpha(c) || c == '_') {
                string id = readIdentifier();
                auto m = keywordBor.longestMatch(id, 0);
                TokenType tt = (m.first == (int)id.size()) ? TokenType::Keyword : TokenType::Identifier;
                tokens.push_back({ tt, id, startLine, startCol });
                continue;
            }
            // оператор; пунктуация
            {
                auto m = opBor.longestMatch(src, pos);
                if (m.first > 0) {
                    string op = m.second;
                    nextChar(m.first);
                    TokenType tt = isPunctuation(op) ? TokenType::Punctuation : TokenType::Operator;
                    tokens.push_back({ tt, op, startLine, startCol });
                    continue;
                }
            }
            // неизвестный символ
            string s(1, c);
            nextChar(1);
            tokens.push_back({ TokenType::Unknown, s, startLine, startCol });
        }
        return tokens;
    }

private:
    string src;
    int pos;
    int line, col;
    Bor keywordBor;
    Bor opBor;

    bool eof() const { return pos >= (int)src.size(); }
    char curChar(int ahead = 0) const { return (pos + ahead < (int)src.size()) ? src[pos + ahead] : '\0'; }
    void nextChar(int n = 1) {
        for (int i = 0; i < n && pos < (int)src.size(); ++i) {
            if (src[pos] == '\n') { ++line; col = 1; }
            else ++col;
            ++pos;
        }
    }
    void skipWhitespace() {
        while (!eof() && isspace(curChar())) nextChar();
    }
    string readUntil(char terminator) {
        string out;
        while (!eof()) {
            char c = curChar();
            out.push_back(c);
            nextChar();
            if (c == terminator) break;
        }
        return out;
    }
    string readBlockComment() {
        string out;
        if (curChar() == '/' && curChar(1) == '*') {
            out += "/*"; nextChar(2);
        }
        while (!eof()) {
            char c = curChar(); out.push_back(c); nextChar();
            if (c == '*' && curChar() == '/') {
                out.push_back('/'); nextChar(); break;
            }
        }
        return out;
    }
    string readStringLiteral() {
        string out; out.push_back('"'); nextChar();
        while (!eof()) {
            char c = curChar(); out.push_back(c); nextChar();
            if (c == '\\' && !eof()) { out.push_back(curChar()); nextChar(); continue; }
            if (c == '"') break;
        }
        return out;
    }
    string readCharLiteral() {
        string out; out.push_back('\''); nextChar();
        while (!eof()) {
            char c = curChar(); out.push_back(c); nextChar();
            if (c == '\\' && !eof()) { out.push_back(curChar()); nextChar(); continue; }
            if (c == '\'') break;
        }
        return out;
    }
    Token readNumber() {
        int startPos = pos;
        bool isFloat = false;
        if (curChar() == '0' && (curChar(1) == 'x' || curChar(1) == 'X')) {
            nextChar(2);
            while (isxdigit(curChar())) nextChar();
            string lex = src.substr(startPos, pos - startPos);
            return { TokenType::IntegerLiteral, lex, line, col };
        }
        while (isdigit(curChar())) nextChar();
        if (curChar() == '.' && isdigit(curChar(1))) {
            isFloat = true; nextChar();
            while (isdigit(curChar())) nextChar();
        }
        if (curChar() == 'e' || curChar() == 'E') {
            isFloat = true; nextChar();
            if (curChar() == '+' || curChar() == '-') nextChar();
            while (isdigit(curChar())) nextChar();
        }
        while (isalpha(curChar())) {
            char c = curChar();
            if (strchr("uUlLfF", c)) nextChar();
            else break;
        }
        string lex = src.substr(startPos, pos - startPos);
        return { isFloat ? TokenType::FloatLiteral : TokenType::IntegerLiteral, lex, line, col };
    }
    string readIdentifier() {
        int start = pos; nextChar();
        while (isalnum(curChar()) || curChar() == '_') nextChar();
        return src.substr(start, pos - start);
    }
    bool isPunctuation(const string& s) const {
        const char* punct[] = {
            "(", ")", "{", "}", "[", "]", ";", ",", ":", "?", "~", ".", "->", "::", nullptr
        };
        for (int i = 0; punct[i]; ++i) {
            if (s == punct[i]) return true;
        }
        return false;
    }
    void buildKeywordBor() {
        vector <string> keywords = {
            "and", "auto","bool","break","case","catch","char",
            "class","const", "continue","default","delete","do","double","else",
            "false","float","for","friend","goto","if",
            "int","long","namespace","new","not","nullptr","operator","or","private",
            "protected","public","return","short","signed","sizeof","static",
            "struct","switch","template","this","throw","true","try","typedef","typename",
            "unsigned","using","virtual","void","while","xor"
        };
        for (auto& k : keywords) keywordBor.insert(k);
    }
    void buildOperatorBor() {
        vector <string> ops = {
            ">>=", "<<=", "->*", "->", "++", "--", "==", "!=", "<=", ">=", "&&", "||",
            "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "##", "::", ".*", "<<", ">>",
            "+", "-", "*", "/", "%", "&", "|", "^", "!", "~", "=", "<", ">", "(", ")", "{", "}", "[", "]", ";", ",", ".", ":", "?", "#"
        };
        for (auto& o : ops) opBor.insert(o);
    }
};

int main() {
    ifstream in("in.txt");
    ostringstream ss;
    ss << in.rdbuf();
    string source = ss.str();
    in.close();

    Lexer lexer(source);
    auto tokens = lexer.tokenize();

    ofstream out("out.txt");
    for (auto& t : tokens) {
        out << left << setw(15) << tokenTypeName(t.type) << " : " << t.lexeme << "\n";
    }
    out.close();
    cout << "Lexical analyzer is finished. View reults in out.txt\n";
    return 0;
}