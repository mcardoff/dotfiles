class Scheme(Default):
    progress_bar_color = orange
    def use(self, context):
        fg, bg, attr = Default.use(self, context)

        if context.directory and not context.marked and not context.link:
            fg = red

        if context.in_titlebar and context.hostname:
            fg = red if context.bad else blue

        return fg, bg, attr
