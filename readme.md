# MarkMark Markup Translator

an opinionated substitute to pandoc, providing a codec between [Markdown](https://daringfireball.net/projects/markdown/) and [Beamer](https://ctan.org/pkg/beamer).

assumes inputs with the following markdown structure


```
# Presentation Title

Name

Institute

## Section Title

### Subsection Title

#### Slide Title

- Bullet point
  - Nested bullet point
```

and converts it into beamer content

```latex

\title{Title}

\author[Author]{Author}

\institute[Institute]{Institute}

\begin{document}

\section{Section Title}

\subsection{Subsection Title} 

\begin{frame}{Slide Title}
    \begin{itemize}
        \item Bullet point
        \begin{itemize}
            \item Nested Bullet Point
        \end{itemize}
    \end{itemize}
\end{frame}
```

or it goes the other way too.