#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
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

//бор
struct TrieNode {
    unordered_map<char, TrieNode*> next;
    bool terminal = false;
    string value;
};

class Trie {
public:
    Trie() { root = new TrieNode(); }
    ~Trie() { clear(root); }

    void insert(const string& s) {
        TrieNode* cur = root;
        for (char c : s) {
            if (!cur->next.count(c)) cur->next[c] = new TrieNode();
            cur = cur->next[c];
        }
        cur->terminal = true;
        cur->value = s;
    }

    pair<int, string> longestMatch(const string& s, int pos) const {
        TrieNode* cur = root;
        int bestLen = 0;
        string bestVal;
        int i = pos;
        while (i < (int)s.size()) {
            char c = s[i];
            if (!cur->next.count(c)) break;
            cur = cur->next.at(c);
            ++i;
            if (cur->terminal) {
                bestLen = i - pos;
                bestVal = cur->value;
            }
        }
        return { bestLen, bestVal };
    }

private:
    TrieNode* root;
    void clear(TrieNode* node) {
        if (!node) return;
        for (auto& p : node->next) clear(p.second);
        delete node;
    }
};

// лексер
class Lexer {
public:
    Lexer(const string& src) : src(src), pos(0), line(1), col(1) {
        buildKeywordTrie();
        buildOperatorTrie();
    }

    vector<Token> tokenize() {
        vector<Token> tokens;
        while (true) {
            skipWhitespace();
            if (eof()) {
                tokens.push_back({ TokenType::EndOfFile, "", line, col });
                break;
            }
            int startLine = line, startCol = col;
            char c = peek();

            if (c == '#' && (col == 1)) {
                string pp = readUntil('\n');
                tokens.push_back({ TokenType::Preprocessor, pp, startLine, startCol });
                continue;
            }

            // коммент
            if (c == '/') {
                if (peek(1) == '/') {
                    string com = readUntil('\n');
                    tokens.push_back({ TokenType::Comment, com, startLine, startCol });
                    continue;
                }
                else if (peek(1) == '*') {
                    string com = readBlockComment();
                    tokens.push_back({ TokenType::Comment, com, startLine, startCol });
                    continue;
                }
            }

            // строковый литерал
            if (c == '"') {
                string s = readStringLiteral();
                tokens.push_back({ TokenType::StringLiteral, s, startLine, startCol });
                continue;
            }

            // символ литерал
            if (c == '\'') {
                string s = readCharLiteral();
                tokens.push_back({ TokenType::CharLiteral, s, startLine, startCol });
                continue;
            }

            // число
            if (isdigit(c) || (c == '.' && isdigit(peek(1)))) {
                Token t = readNumber();
                t.line = startLine; t.column = startCol;
                tokens.push_back(t);
                continue;
            }

            // идентификаторы; зарезервированные слова
            if (isalpha(c) || c == '_') {
                string id = readIdentifier();
                auto m = keywordTrie.longestMatch(id, 0);
                TokenType tt = (m.first == (int)id.size()) ? TokenType::Keyword : TokenType::Identifier;
                tokens.push_back({ tt, id, startLine, startCol });
                continue;
            }

            // оператор; пунктуация
            {
                auto m = opTrie.longestMatch(src, pos);
                if (m.first > 0) {
                    string op = m.second;
                    advance(m.first);
                    TokenType tt = isPunctuation(op) ? TokenType::Punctuation : TokenType::Operator;
                    tokens.push_back({ tt, op, startLine, startCol });
                    continue;
                }
            }

            // неизвестный символ
            string s(1, c);
            advance(1);
            tokens.push_back({ TokenType::Unknown, s, startLine, startCol });
        }
        return tokens;
    }

private:
    string src;
    int pos;
    int line, col;
    Trie keywordTrie;
    Trie opTrie;

    bool eof() const { return pos >= (int)src.size(); }
    char peek(int ahead = 0) const { return (pos + ahead < (int)src.size()) ? src[pos + ahead] : '\0'; }
    void advance(int n = 1) {
        for (int i = 0;i < n && pos < (int)src.size();++i) {
            if (src[pos] == '\n') { ++line; col = 1; }
            else ++col;
            ++pos;
        }
    }
    void skipWhitespace() {
        while (!eof() && isspace(peek())) advance();
    }

    string readUntil(char terminator) {
        string out;
        while (!eof()) {
            char c = peek();
            out.push_back(c);
            advance();
            if (c == terminator) break;
        }
        return out;
    }

    string readBlockComment() {
        string out;
        if (peek() == '/' && peek(1) == '*') {
            out += "/*"; advance(2);
        }
        while (!eof()) {
            char c = peek(); out.push_back(c); advance();
            if (c == '*' && peek() == '/') {
                out.push_back('/'); advance(); break;
            }
        }
        return out;
    }

    string readStringLiteral() {
        string out; out.push_back('"'); advance();
        while (!eof()) {
            char c = peek(); out.push_back(c); advance();
            if (c == '\\' && !eof()) { out.push_back(peek()); advance(); continue; }
            if (c == '"') break;
        }
        return out;
    }

    string readCharLiteral() {
        string out; out.push_back('\''); advance();
        while (!eof()) {
            char c = peek(); out.push_back(c); advance();
            if (c == '\\' && !eof()) { out.push_back(peek()); advance(); continue; }
            if (c == '\'') break;
        }
        return out;
    }

    Token readNumber() {
        int startPos = pos;
        bool isFloat = false;
        if (peek() == '0' && (peek(1) == 'x' || peek(1) == 'X')) {
            advance(2);
            while (isxdigit(peek())) advance();
            string lex = src.substr(startPos, pos - startPos);
            return { TokenType::IntegerLiteral, lex, line, col };
        }
        while (isdigit(peek())) advance();
        if (peek() == '.' && isdigit(peek(1))) {
            isFloat = true; advance();
            while (isdigit(peek())) advance();
        }
        if (peek() == 'e' || peek() == 'E') {
            isFloat = true; advance();
            if (peek() == '+' || peek() == '-') advance();
            while (isdigit(peek())) advance();
        }
        while (isalpha(peek())) {
            char c = peek();
            if (strchr("uUlLfF", c)) advance();
            else break;
        }
        string lex = src.substr(startPos, pos - startPos);
        return { isFloat ? TokenType::FloatLiteral : TokenType::IntegerLiteral, lex, line, col };
    }

    string readIdentifier() {
        int start = pos; advance();
        while (isalnum(peek()) || peek() == '_') advance();
        return src.substr(start, pos - start);
    }

    bool isPunctuation(const string& s) const {
        static const unordered_set<string> punct = {
            "(", ")", "{", "}", "[", "]", ";", ",", ":", "?", "~", ".", "->", "::"
        };
        return punct.count(s) > 0;
    }

    void buildKeywordTrie() {
        vector<string> keywords = {
            "alignas","alignof","and","and_eq","asm","auto","bool","break","case","catch","char",
            "class","const","constexpr","continue","decltype","default","delete","do","double","else",
            "enum","explicit","export","extern","false","float","for","friend","goto","if","inline",
            "int","long","mutable","namespace","new","noexcept","not","nullptr","operator","or","private",
            "protected","public","register","reinterpret_cast","return","short","signed","sizeof","static",
            "struct","switch","template","this","throw","true","try","typedef","typename","union",
            "unsigned","using","virtual","void","volatile","while","xor"
        };
        for (auto& k : keywords) keywordTrie.insert(k);
    }

    void buildOperatorTrie() {
        vector<string> ops = {
            ">>=", "<<=", "->*", "->", "++", "--", "==", "!=", "<=", ">=", "&&", "||",
            "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "##", "::", ".*", "<<", ">>",
            "+", "-", "*", "/", "%", "&", "|", "^", "!", "~", "=", "<", ">", "(", ")", "{", "}", "[", "]", ";", ",", ".", ":", "?", "#"
        };
        for (auto& o : ops) opTrie.insert(o);
    }
};


int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    ifstream in("in.txt");
    if (!in) {
        cerr << "Error: file is not found in.txt\n";
        return 1;
    }

    ostringstream ss;
    ss << in.rdbuf();
    string source = ss.str();
    in.close();

    Lexer lexer(source);
    auto tokens = lexer.tokenize();

    ofstream out("out.txt");
    if (!out) {
        cerr << "Error: cannot open out.txt for writing\n";
        return 1;
    }

    for (auto& t : tokens) {
        out << "[" << setw(4) << t.line << ":" << setw(3) << t.column << "] "
            << left << setw(15) << tokenTypeName(t.type)
            << " : " << t.lexeme << "\n";
    }

    out.close();
    cout << "Lexical analyzer is finished. View reults in out.txt\n";
    return 0;
}
