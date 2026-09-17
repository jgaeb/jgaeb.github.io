# Tools

Notes on tooling availability in the `/workspace` container.

## Missing

- **Ruby / Bundler / Jekyll** — no `ruby`, `gem`, `bundle`, or `jekyll` on PATH.
  The site is a Jekyll project (see `Gemfile`), but it cannot be built or served
  from this container. Build/preview locally with `bundle exec jekyll serve`.
