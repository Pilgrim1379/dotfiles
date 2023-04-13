IEx.configure(
  colors: [
    ls_directory: :cyan,
    ls_device: :yellow,
    doc_code: :green,
    doc_inline_code: :magenta,
    doc_headings: [:cyan, :underline],
    doc_title: [:cyan, :bright, :underline]
  ],
  default_prompt:
    "#{IO.ANSI.green()}%prefix#{IO.ANSI.reset()}" <>
      "(#{IO.ANSI.cyan()}%counter#{IO.ANSI.reset()})#{IO.ANSI.green()}❯#{IO.ANSI.reset()}",
  alive_prompt:
    "#{IO.ANSI.green()}%prefix#{IO.ANSI.reset()}" <>
      "[#{IO.ANSI.yellow()}%node#{IO.ANSI.reset()}]" <>
      "(#{IO.ANSI.cyan()}%counter#{IO.ANSI.reset()})#{IO.ANSI.green()}❯#{IO.ANSI.reset()}",
  history_size: 50,
  inspect: [
    pretty: true,
    limit: :infinity,
    width: 80
  ],
  width: 80
)
