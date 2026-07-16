# Princeton Sociology Methods Camp website

This repository contains the one-page website for the Princeton Sociology Methods Camp:

https://pusocmethodscamp.org/

The site is built with [Quarto](https://quarto.org/) and published through GitHub Pages.

## Repository structure

- `index.qmd` assembles the scrolling homepage.
- `sections/` contains the editable Markdown sections included by the homepage.
- `download/` contains the current assignments, slides, and data files.
- `styles.scss` contains the small set of site-specific style rules.
- `nav-active.html` highlights the current section in the navigation while scrolling.
- `_quarto.yml` contains navigation and build settings.
- `.github/workflows/site.yml` validates pull requests and deploys `master`.

Only `index.qmd` is rendered as a page. Files under `sections/` are included as fragments, and files under `download/` are copied without being executed.

## Local preview

Install Quarto 1.9.38 or a compatible newer stable release. From the repository root, run:

```sh
quarto preview
```

The preview opens in a browser and refreshes as files change. Before opening a pull request, run a clean production render:

```sh
quarto render
```

Generated files are written to `_site/` and are not committed.

## Publishing

Every pull request to `master` must render successfully and pass the automated link check. After a pull request is merged, the same workflow renders the site and deploys it to GitHub Pages. Do not edit or commit `_site/`.

The repository has two one-time settings that support this workflow:

- GitHub Pages uses **GitHub Actions** as its source.
- The `master` branch requires a pull request and the `render-and-check` status before merging.

## Annual update checklist

1. Update the introductory text in `index.qmd`.
2. Update the objectives, pre-arrival information, schedule, FAQs, and acknowledgements under `sections/`.
3. Replace the current student-facing files under `download/`; remove files that should no longer be distributed.
4. Run `quarto preview` and inspect the complete page at desktop and mobile widths.
5. Check every student-facing download.
6. Run `quarto render` and confirm there are no warnings or errors.
7. Open a pull request and wait for the `render-and-check` status to pass.
8. Merge the pull request and verify https://pusocmethodscamp.org/.

## Troubleshooting

- If a section is missing, check its include line in `index.qmd` and its filename under `sections/`.
- If a navigation link misses its section, verify that its anchor in `_quarto.yml` matches the rendered heading identifier.
- If the link check fails for a temporary external error, retry the workflow once before changing or excluding the link.
- If deployment fails after a successful render, inspect the `deploy` job in the GitHub Actions run.
