// Generator for the TextMate grammar.
// Running with `bun grammar.ts` will print a JSON string suitable for being written in a `.tmLanguage.json` file
const r = String.raw;

// A tiny shallowly-embedded regex DSL
const anyOf = (...alternatives: string[]) => `(?:${alternatives.join('|')})`;
const optional = (pattern: string) => `(?:${pattern})?`;
const many = (pattern: string) => `(?:${pattern})*`;
const some = (pattern: string) => `(?:${pattern})+`;
const notFollowedBy = (pattern: string) => `(?!${pattern})`;
const capture = (pattern: string) => `(${pattern})`;

const opChar = r`[!#$%&*+/\\<>?@^|\-~=.]`;
const identChar = '[a-zA-Z0-9_]';
const typeIdentifier = `[A-Z]${many(identChar)}`;
const variableIdentifier = `[a-z_]${many(identChar)}`;
const operator = some(opChar);
const qualification = some(r`${typeIdentifier}\.`);
const qualifiedVariable = qualification + variableIdentifier;
const qualifiedType = qualification + typeIdentifier;
const qualifiedOperator = qualification + operator;

// stop reading at the end of a word, so e.g. `letter` isn't lexed as `let`
const exactly = (pattern: string) => pattern + notFollowedBy(identChar);

const keywords = {
  control: ['match', 'then', 'else', 'with', 'if', 'in'],
  declaration: ['class', 'alias', 'data', 'type', 'let', 'def'],
  import: ['qualified', 'module', 'import', 'hiding'],
};
const allKeywords = anyOf(...Object.values(keywords).flat());

const digits = some('[0-9]');
const octal = `0[oO]${some('[0-7]')}`;
const hex = `0[xX]${some('[0-9a-fA-F]')}`;
const exponent = `[eE]${optional('[-+]')}${digits}`;
const float = anyOf(r`${digits}\.${digits}${optional(exponent)}`, digits + exponent);
const int = anyOf(octal, hex, digits);

const asciiEscapes =
  'NUL SOH STX ETX EOT ENQ ACK BEL BS HT LF VT FF CR SO SI DLE DC1 DC2 DC3 DC4 NAK SYN ETB CAN EM SUB ESC FS GS RS US SP DEL';

const charEscape = r`\\${anyOf(
  `o${some('[0-7]')}`,
  `x${some('[0-9a-fA-F]')}`,
  r`[abfnrtv\\"']`,
  r`\^[A-Z@\[\\\]^_]`,
  ...asciiEscapes.split(' '),
  digits,
)}`;

// the body of a char literal: a space, a normal character that doesn't need to be escaped, or an escape code
const charBody = anyOf(' ', notFollowedBy(r`['\\]`) + r`[^\n]`, charEscape);

const definitionKeyword = anyOf('let!', exactly('let'), exactly('def'));

// an operator name in the context of a definition, e.g. `(<>)` in `let (<>) = ...`
const operatorDefinition = r`\(${capture(operator)}\)`;
// a variable name in the context of a definition that can start with `_`, cannot be a keyword, but otherwise can be any variableIdentifier

const variableDefinition = capture(
  notFollowedBy(exactly('_')) + notFollowedBy(exactly(allKeywords)) + variableIdentifier,
);

// TextMate scope
type Scope = `${string}.elara`;
const scope = (name: string): Scope => `${name}.elara`;

type Rule<K extends string> =
  | { include: `#${K}`; name?: never; match?: never; begin?: never }
  | { match: string; name?: Scope; captures?: Record<number, { name: Scope }>; include?: never; begin?: never }
  | { begin: string; end: string; name: Scope; patterns?: Rule<K>[]; include?: never; match?: never };

interface Grammar<K extends string> {
  scopeName: `source.${string}`;
  name: string;
  fileTypes: string[];
  patterns: Rule<K>[];
  repository: Record<K, Rule<K>>;
}

const defineGrammar = <K extends string>(grammar: Grammar<K>) => grammar;

const grammar = defineGrammar({
  scopeName: 'source.elara',
  name: 'elara',
  fileTypes: ['elr'],
  patterns: [
    { name: scope('comment.line.double-dash'), match: '--' + many(r`[^\n]`) },
    { include: '#block-comment' },
    {
      name: scope('string.quoted.double'),
      begin: '"',
      end: '"',
      patterns: [{ name: scope('constant.character.escape'), match: r`\\.` }],
    },
    { name: scope('string.quoted.single'), match: `'${charBody}'` },
    { name: scope('constant.numeric'), match: `-?${float}` },
    { name: scope('constant.numeric'), match: `-?${int}` },
    {
      name: scope('entity.other.attribute-name.annotation'),
      match: '#' + notFollowedBy(qualifiedOperator) + anyOf(qualifiedVariable, qualifiedType, typeIdentifier),
    },
    {
      match: capture(definitionKeyword) + some(r`\s`) + anyOf(operatorDefinition, variableDefinition),
      captures: {
        1: { name: scope('keyword.declaration') },
        2: { name: scope('entity.name.function') },
        3: { name: scope('entity.name.function') },
      },
    },
    { name: scope('keyword.control'), match: exactly(anyOf(...keywords.control)) },
    { name: scope('keyword.declaration'), match: anyOf('let!', exactly(anyOf(...keywords.declaration))) },
    { name: scope('keyword.other.import'), match: exactly(anyOf(...keywords.import)) },
    { name: scope('variable.language.wildcard'), match: exactly('_') },
    { name: scope('keyword.operator'), match: qualifiedOperator },
    { match: qualifiedVariable },
    { name: scope('support.type'), match: qualifiedType },
    { name: scope('keyword.operator.arrow'), match: anyOf('->', '<-', '=>') + notFollowedBy(opChar) },
    { name: scope('keyword.operator'), match: ':' + optional(':') },
    { name: scope('keyword.operator'), match: operator },
    { name: scope('support.type'), match: typeIdentifier },
    { match: variableIdentifier },
  ],
  repository: {
    'block-comment': {
      name: scope('comment.block'),
      begin: '/-' + notFollowedBy(opChar),
      end: '-/',
      patterns: [{ include: '#nested-block-comment' }],
    },
    'nested-block-comment': {
      name: scope('comment.block'),
      begin: '/-',
      end: '-/',
      patterns: [{ include: '#nested-block-comment' }],
    },
  },
});

export default grammar;

export const grammarJson = JSON.stringify(grammar, null, 2);

if (import.meta.main) console.log(grammarJson);
