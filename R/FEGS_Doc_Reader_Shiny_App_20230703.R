# Define UI for the FEGS Doc Reader  ----

#' FEGS document reader application
#' 
#' @export fegs_app
fegs_app <- function() {
  ui <- shiny::fluidPage(
    

    htmltools::tags$html(class = "no-js", lang="en"),
    htmltools::tags$head(
      htmltools::HTML(
        "<!-- Google Tag Manager -->
		<script>(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
		new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
		j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
		'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
		})(window,document,'script','dataLayer','GTM-L8ZB');</script>
		<!-- End Google Tag Manager -->
		"
      ),
      htmltools::tags$meta(charset="utf-8"),
      htmltools::tags$meta(property="og:site_name", content="US EPA"),
      #tags$link(rel = "stylesheet", type = "text/css", href = "css/uswds.css"),
      htmltools::tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/uswds/3.0.0-beta.3/css/uswds.min.css", integrity="sha512-ZKvR1/R8Sgyx96aq5htbFKX84hN+zNXN73sG1dEHQTASpNA8Pc53vTbPsEKTXTZn9J4G7R5Il012VNsDEReqCA==", crossorigin="anonymous", referrerpolicy="no-referrer"),
      htmltools::tags$meta(property="og:url", content="https://www.epa.gov/themes/epa_theme/pattern-lab/.markup-only.html"),
      htmltools::tags$link(rel="canonical", href="https://www.epa.gov/themes/epa_theme/pattern-lab/.markup-only.html"),
      htmltools::tags$link(rel="shortlink", href="https://www.epa.gov/themes/epa_theme/pattern-lab/.markup-only.html"),
      htmltools::tags$meta(property="og:url", content="https://www.epa.gov/themes/epa_theme/pattern-lab/.markup-only.html"),
      htmltools::tags$meta(property="og:image", content="https://www.epa.gov/sites/all/themes/epa/img/epa-standard-og.jpg"),
      htmltools::tags$meta(property="og:image:width", content="1200"),
      htmltools::tags$meta(property="og:image:height", content="630"),
      htmltools::tags$meta(property="og:image:alt", content="U.S. Environmental Protection Agency"),
      htmltools::tags$meta(name="twitter:card", content="summary_large_image"),
      htmltools::tags$meta(name="twitter:image:alt", content="U.S. Environmental Protection Agency"),
      htmltools::tags$meta(name="twitter:image:height", content="600"),
      htmltools::tags$meta(name="twitter:image:width", content="1200"),
      htmltools::tags$meta(name="twitter:image", content="https://www.epa.gov/sites/all/themes/epa/img/epa-standard-twitter.jpg"),
      htmltools::tags$meta(name="MobileOptimized", content="width"),
      htmltools::tags$meta(name="HandheldFriendly", content="true"),
      htmltools::tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
      htmltools::tags$meta(`http-equiv`="x-ua-compatible", content="ie=edge"),
      htmltools::tags$script(src = "js/pattern-lab-head-script.js"),
      # update this line with your title:    tags$title('ContDataQC | US EPA'),
      htmltools::tags$link(rel="icon", type="image/x-icon", href="https://www.epa.gov/themes/epa_theme/images/favicon.ico"),
      htmltools::tags$meta(name="msapplication-TileColor", content="#FFFFFF"),
      htmltools::tags$meta(name="msapplication-TileImage", content="https://www.epa.gov/themes/epa_theme/images/favicon-144.png"),
      htmltools::tags$meta(name="application-name", content=""),
      htmltools::tags$meta(name="msapplication-config", content="https://www.epa.gov/themes/epa_theme/images/ieconfig.xml"),
      htmltools::tags$link(rel="apple-touch-icon-precomposed", sizes="196x196", href="https://www.epa.gov/themes/epa_theme/images/favicon-196.png"),
      htmltools::tags$link(rel="apple-touch-icon-precomposed", sizes="152x152", href="https://www.epa.gov/themes/epa_theme/images/favicon-152.png"),
      htmltools::tags$link(rel="apple-touch-icon-precomposed", sizes="144x144", href="https://www.epa.gov/themes/epa_theme/images/favicon-144.png"),
      htmltools::tags$link(rel="apple-touch-icon-precomposed", sizes="120x120", href="https://www.epa.gov/themes/epa_theme/images/favicon-120.png"),
      htmltools::tags$link(rel="apple-touch-icon-precomposed", sizes="114x114", href="https://www.epa.gov/themes/epa_theme/images/favicon-114.png"),
      htmltools::tags$link(rel="apple-touch-icon-precomposed", sizes="72x72", href="https://www.epa.gov/themes/epa_theme/images/favicon-72.png"),
      htmltools::tags$link(rel="apple-touch-icon-precomposed", href="https://www.epa.gov/themes/epa_theme/images/favicon-180.png"),
      htmltools::tags$link(rel="icon", href="https://www.epa.gov/themes/epa_theme/images/favicon-32.png", sizes="32x32"),
      htmltools::tags$link(rel="preload", href="https://www.epa.gov/themes/epa_theme/fonts/source-sans-pro/sourcesanspro-regular-webfont.woff2", as="font", crossorigin="anonymous"),
      htmltools::tags$link(rel="preload", href="https://www.epa.gov/themes/epa_theme/fonts/source-sans-pro/sourcesanspro-bold-webfont.woff2", as="font", crossorigin="anonymous"),
      htmltools::tags$link(rel="preload", href="https://www.epa.gov/themes/epa_theme/fonts/merriweather/Latin-Merriweather-Bold.woff2", as="font", crossorigin="anonymous"),
      htmltools::tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/ajax-progress.module.css?r6lsex"),
      htmltools::tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/autocomplete-loading.module.css?r6lsex" ),
      htmltools::tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/js.module.css?r6lsex"),
      htmltools::tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/sticky-header.module.css?r6lsex"),
      htmltools::tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/system-status-counter.css?r6lsex"),
      htmltools::tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/system-status-report-counters.css?r6lsex"),
      htmltools::tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/system-status-report-general-info.css?r6lsex"),
      htmltools::tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/tabledrag.module.css?r6lsex"),
      htmltools::tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/tablesort.module.css?r6lsex"),
      htmltools::tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/tree-child.module.css?r6lsex"),
      htmltools::tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/themes/epa_theme/css/styles.css?r6lsex"),
      htmltools::tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/themes/epa_theme/css-lib/colorbox.min.css?r6lsex"),
      
      htmltools::tags$script(src = 'https://cdnjs.cloudflare.com/ajax/libs/uswds/3.0.0-beta.3/js/uswds-init.min.js'),
      #fix container-fluid that boostrap RShiny uses
      htmltools::tags$style(htmltools::HTML(
        '.container-fluid {
            padding-right: 0;
            padding-left: 0;
            margin-right: 0;
            margin-left: 0;
        }
        .tab-content {
            margin-right: 30px;
            margin-left: 30px;
        }'
      ))
    ),
    htmltools::tags$body(
      class="path-themes not-front has-wide-template", id="top",
      htmltools::tags$script(
        src = 'https://cdnjs.cloudflare.com/ajax/libs/uswds/3.0.0-beta.3/js/uswds.min.js'
      )
    ),
    
    # Site Header
    htmltools::HTML(
      '<div class="skiplinks" role="navigation" aria-labelledby="skip-to-main">
      <a id="skip-to-main" href="#main" class="skiplinks__link visually-hidden focusable">Skip to main content</a>
    </div>

	<!-- Google Tag Manager (noscript) -->
	<noscript><iframe src=https://www.googletagmanager.com/ns.html?id=GTM-L8ZB
	height="0" width="0" style="display:none;visibility:hidden"></iframe></noscript>
	<!-- End Google Tag Manager (noscript) -->

    <div class="dialog-off-canvas-main-canvas" data-off-canvas-main-canvas>
    <section class="usa-banner" aria-label="Official government website">
      <div class="usa-accordion">
        <header class="usa-banner__header">
          <div class="usa-banner__inner">
            <div class="grid-col-auto">
              <img class="usa-banner__header-flag" src="https://www.epa.gov/themes/epa_theme/images/us_flag_small.png" alt="U.S. flag" />
            </div>
            <div class="grid-col-fill tablet:grid-col-auto">
              <p class="usa-banner__header-text">An official website of the United States government</p>
              <p class="usa-banner__header-action" aria-hidden="true">Here’s how you know</p>
            </div>
            <button class="usa-accordion__button usa-banner__button" aria-expanded="false" aria-controls="gov-banner">
              <span class="usa-banner__button-text">Here’s how you know</span>
            </button>
          </div>
        </header>
        <div class="usa-banner__content usa-accordion__content" id="gov-banner">
          <div class="grid-row grid-gap-lg">
            <div class="usa-banner__guidance tablet:grid-col-6">
              <img class="usa-banner__icon usa-media-block__img" src="https://www.epa.gov/themes/epa_theme/images/icon-dot-gov.svg" alt="Dot gov">
              <div class="usa-media-block__body">
                <p>
                  <strong>Official websites use .gov</strong>
                  <br> A <strong>.gov</strong> website belongs to an official government organization in the United States.
                </p>
              </div>
            </div>
            <div class="usa-banner__guidance tablet:grid-col-6">
              <img class="usa-banner__icon usa-media-block__img" src="https://www.epa.gov/themes/epa_theme/images/icon-https.svg" alt="HTTPS">
              <div class="usa-media-block__body">
                <p>
                  <strong>Secure .gov websites use HTTPS</strong>
                  <br> A <strong>lock</strong> (<span class="icon-lock"><svg xmlns="http://www.w3.org/2000/svg" width="52" height="64" viewBox="0 0 52 64" class="usa-banner__lock-image" role="img" aria-labelledby="banner-lock-title banner-lock-description"><title id="banner-lock-title">Lock</title><desc id="banner-lock-description">A locked padlock</desc><path fill="#000000" fill-rule="evenodd" d="M26 0c10.493 0 19 8.507 19 19v9h3a4 4 0 0 1 4 4v28a4 4 0 0 1-4 4H4a4 4 0 0 1-4-4V32a4 4 0 0 1 4-4h3v-9C7 8.507 15.507 0 26 0zm0 8c-5.979 0-10.843 4.77-10.996 10.712L15 19v9h22v-9c0-6.075-4.925-11-11-11z"/></svg></span>) or <strong>https://</strong> means you’ve safely connected to the .gov website. Share sensitive information only on official, secure websites.
                </p>
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>
    <div>
      <div class="js-view-dom-id-epa-alerts--public">
        <noscript>
          <div class="usa-site-alert usa-site-alert--info">
            <div class="usa-alert">
              <div class="usa-alert__body">
                <div class="usa-alert__text">
                  <p>JavaScript appears to be disabled on this computer. Please <a href="/alerts">click here to see any active alerts</a>.</p>
                </div>
              </div>
            </div>
          </div>
        </noscript>
      </div>
    </div>
    <header class="l-header">
      <div class="usa-overlay"></div>
      <div class="l-constrain">
        <div class="l-header__navbar">
          <div class="l-header__branding">
            <a class="site-logo" href="/" aria-label="Home" title="Home" rel="home">
              <span class="site-logo__image">
                <svg class="site-logo__svg" viewBox="0 0 1061 147" aria-hidden="true" xmlns="http://www.w3.org/2000/svg">
                  <path d="M112.8 53.5C108 72.1 89.9 86.8 69.9 86.8c-20.1 0-38-14.7-42.9-33.4h.2s9.8 10.3-.2 0c3.1 3.1 6.2 4.4 10.7 4.4s7.7-1.3 10.7-4.4c3.1 3.1 6.3 4.5 10.9 4.4 4.5 0 7.6-1.3 10.7-4.4 3.1 3.1 6.2 4.4 10.7 4.4 4.5 0 7.7-1.3 10.7-4.4 3.1 3.1 6.3 4.5 10.9 4.4 4.3 0 7.4-1.2 10.5-4.3zM113.2 43.5c0-24-19.4-43.5-43.3-43.5-24 0-43.5 19.5-43.5 43.5h39.1c-4.8-1.8-8.1-6.3-8.1-11.6 0-7 5.7-12.5 12.5-12.5 7 0 12.7 5.5 12.7 12.5 0 5.2-3.1 9.6-7.6 11.6h38.2zM72.6 139.3c.7-36.9 29.7-68.8 66.9-70 0 37.2-30 68-66.9 70zM67.1 139.3c-.7-36.9-29.7-68.8-67.1-70 0 37.2 30.2 68 67.1 70zM240 3.1h-87.9v133.1H240v-20.4h-60.3v-36H240v-21h-60.3v-35H240V3.1zM272.8 58.8h27.1c9.1 0 15.2-8.6 15.1-17.7-.1-9-6.1-17.3-15.1-17.3h-25.3v112.4h-27.8V3.1h62.3c20.2 0 35 17.8 35.2 38 .2 20.4-14.8 38.7-35.2 38.7h-36.3v-21zM315.9 136.2h29.7l12.9-35h54.2l-8.1-21.9h-38.4l18.9-50.7 39.2 107.6H454L400.9 3.1h-33.7l-51.3 133.1zM473.3.8v22.4c0 1.9.2 3.3.5 4.3s.7 1.7 1 2.2c1.2 1.4 2.5 2.4 3.9 2.9 1.5.5 2.8.7 4.1.7 2.4 0 4.2-.4 5.5-1.3 1.3-.8 2.2-1.8 2.8-2.9.6-1.1.9-2.3 1-3.4.1-1.1.1-2 .1-2.6V.8h4.7v24c0 .7-.1 1.5-.4 2.4-.3 1.8-1.2 3.6-2.5 5.4-1.8 2.1-3.8 3.5-6 4.2-2.2.6-4 .9-5.3.9-1.8 0-3.8-.3-6.2-1.1-2.4-.8-4.5-2.3-6.2-4.7-.5-.8-1-1.8-1.4-3.2-.4-1.3-.6-3.3-.6-5.9V.8h5zM507.5 14.5v-2.9l4.6.1-.1 4.1c.2-.3.4-.7.8-1.2.3-.5.8-.9 1.4-1.4.6-.5 1.4-.9 2.3-1.3.9-.3 2.1-.5 3.4-.4.6 0 1.4.1 2.4.3.9.2 1.9.6 2.9 1.2s1.8 1.5 2.4 2.6c.6 1.2.9 2.8.9 4.7l-.4 17-4.6-.1.4-16c0-.9 0-1.7-.2-2.4-.1-.7-.5-1.3-1.1-1.9-1.2-1.2-2.6-1.8-4.3-1.8-1.7 0-3.1.5-4.4 1.7-1.3 1.2-2 3.1-2.1 5.7l-.3 14.5-4.5-.1.5-22.4zM537.2.9h5.5V6h-5.5V.9m.5 10.9h4.6v25.1h-4.6V11.8zM547.8 11.7h4.3V6.4l4.5-1.5v6.8h5.4v3.4h-5.4v15.1c0 .3 0 .6.1 1 0 .4.1.7.4 1.1.2.4.5.6 1 .8.4.3 1 .4 1.8.4 1 0 1.7-.1 2.2-.2V37c-.9.2-2.1.3-3.8.3-2.1 0-3.6-.4-4.6-1.2-1-.8-1.5-2.2-1.5-4.2V15.1h-4.3v-3.4zM570.9 25.2c-.1 2.6.5 4.8 1.7 6.5 1.1 1.7 2.9 2.6 5.3 2.6 1.5 0 2.8-.4 3.9-1.3 1-.8 1.6-2.2 1.8-4h4.6c0 .6-.2 1.4-.4 2.3-.3 1-.8 2-1.7 3-.2.3-.6.6-1 1-.5.4-1 .7-1.7 1.1-.7.4-1.5.6-2.4.8-.9.3-2 .4-3.3.4-7.6-.2-11.3-4.5-11.3-12.9 0-2.5.3-4.8 1-6.8s2-3.7 3.8-5.1c1.2-.8 2.4-1.3 3.7-1.6 1.3-.2 2.2-.3 3-.3 2.7 0 4.8.6 6.3 1.6s2.5 2.3 3.1 3.9c.6 1.5 1 3.1 1.1 4.6.1 1.6.1 2.9 0 4h-17.5m12.9-3v-1.1c0-.4 0-.8-.1-1.2-.1-.9-.4-1.7-.8-2.5s-1-1.5-1.8-2c-.9-.5-2-.8-3.4-.8-.8 0-1.5.1-2.3.3-.8.2-1.5.7-2.2 1.3-.7.6-1.2 1.3-1.6 2.3-.4 1-.7 2.2-.8 3.6h13zM612.9.9h4.6V33c0 1 .1 2.3.2 4h-4.6l-.1-4c-.2.3-.4.7-.7 1.2-.3.5-.8 1-1.4 1.5-1 .7-2 1.2-3.1 1.4l-1.5.3c-.5.1-.9.1-1.4.1-.4 0-.8 0-1.3-.1s-1.1-.2-1.7-.3c-1.1-.3-2.3-.9-3.4-1.8s-2.1-2.2-2.9-3.8c-.8-1.7-1.2-3.9-1.2-6.6.1-4.8 1.2-8.3 3.4-10.5 2.1-2.1 4.7-3.2 7.6-3.2 1.3 0 2.4.2 3.4.5.9.3 1.6.7 2.2 1.2.6.4 1 .9 1.3 1.4.3.5.6.8.7 1.1V.9m0 23.1c0-1.9-.2-3.3-.5-4.4-.4-1.1-.8-2-1.4-2.6-.5-.7-1.2-1.3-2-1.8-.9-.5-2-.7-3.3-.7-1.7 0-2.9.5-3.8 1.3-.9.8-1.6 1.9-2 3.1-.4 1.2-.7 2.3-.7 3.4-.1 1.1-.2 1.9-.1 2.4 0 1.1.1 2.2.3 3.4.2 1.1.5 2.2 1 3.1.5 1 1.2 1.7 2 2.3.9.6 2 .9 3.3.9 1.8 0 3.2-.5 4.2-1.4 1-.8 1.7-1.8 2.1-3 .4-1.2.7-2.4.8-3.4.1-1.4.1-2.1.1-2.6zM643.9 26.4c0 .6.1 1.3.3 2.1.1.8.5 1.6 1 2.3.5.8 1.4 1.4 2.5 1.9s2.7.8 4.7.8c1.8 0 3.3-.3 4.4-.8 1.1-.5 1.9-1.1 2.5-1.8.6-.7 1-1.5 1.1-2.2.1-.7.2-1.2.2-1.7 0-1-.2-1.9-.5-2.6-.4-.6-.9-1.2-1.6-1.6-1.4-.8-3.4-1.4-5.9-2-4.9-1.1-8.1-2.2-9.5-3.2-1.4-1-2.3-2.2-2.9-3.5-.6-1.2-.8-2.4-.8-3.6.1-3.7 1.5-6.4 4.2-8.1 2.6-1.7 5.7-2.5 9.1-2.5 1.3 0 2.9.2 4.8.5 1.9.4 3.6 1.4 5 3 .5.5.9 1.1 1.2 1.7.3.5.5 1.1.6 1.6.2 1.1.3 2.1.3 2.9h-5c-.2-2.2-1-3.7-2.4-4.5-1.5-.7-3.1-1.1-4.9-1.1-5.1.1-7.7 2-7.8 5.8 0 1.5.5 2.7 1.6 3.5 1 .8 2.6 1.4 4.7 1.9 4 1 6.7 1.8 8.1 2.2.8.2 1.4.5 1.8.7.5.2 1 .5 1.4.9.8.5 1.4 1.1 1.9 1.8s.8 1.4 1.1 2.1c.3 1.4.5 2.5.5 3.4 0 3.3-1.2 6-3.5 8-2.3 2.1-5.8 3.2-10.3 3.3-1.4 0-3.2-.3-5.4-.8-1-.3-2-.7-3-1.2-.9-.5-1.8-1.2-2.5-2.1-.9-1.4-1.5-2.7-1.7-4.1-.3-1.3-.4-2.4-.3-3.2h5zM670 11.7h4.3V6.4l4.5-1.5v6.8h5.4v3.4h-5.4v15.1c0 .3 0 .6.1 1 0 .4.1.7.4 1.1.2.4.5.6 1 .8.4.3 1 .4 1.8.4 1 0 1.7-.1 2.2-.2V37c-.9.2-2.1.3-3.8.3-2.1 0-3.6-.4-4.6-1.2-1-.8-1.5-2.2-1.5-4.2V15.1H670v-3.4zM705.3 36.9c-.3-1.2-.5-2.5-.4-3.7-.5 1-1.1 1.8-1.7 2.4-.7.6-1.4 1.1-2 1.4-1.4.5-2.7.8-3.7.8-2.8 0-4.9-.8-6.4-2.2-1.5-1.4-2.2-3.1-2.2-5.2 0-1 .2-2.3.8-3.7.6-1.4 1.7-2.6 3.5-3.7 1.4-.7 2.9-1.2 4.5-1.5 1.6-.1 2.9-.2 3.9-.2s2.1 0 3.3.1c.1-2.9-.2-4.8-.9-5.6-.5-.6-1.1-1.1-1.9-1.3-.8-.2-1.6-.4-2.3-.4-1.1 0-2 .2-2.6.5-.7.3-1.2.7-1.5 1.2-.3.5-.5.9-.6 1.4-.1.5-.2.9-.2 1.2h-4.6c.1-.7.2-1.4.4-2.3.2-.8.6-1.6 1.3-2.5.5-.6 1-1 1.7-1.3.6-.3 1.3-.6 2-.8 1.5-.4 2.8-.6 4.2-.6 1.8 0 3.6.3 5.2.9 1.6.6 2.8 1.6 3.4 2.9.4.7.6 1.4.7 2 .1.6.1 1.2.1 1.8l-.2 12c0 1 .1 3.1.4 6.3h-4.2m-.5-12.1c-.7-.1-1.6-.1-2.6-.1h-2.1c-1 .1-2 .3-3 .6s-1.9.8-2.6 1.5c-.8.7-1.2 1.7-1.2 3 0 .4.1.8.2 1.3s.4 1 .8 1.5.9.8 1.6 1.1c.7.3 1.5.5 2.5.5 2.3 0 4.1-.9 5.2-2.7.5-.8.8-1.7 1-2.7.1-.9.2-2.2.2-4zM714.5 11.7h4.3V6.4l4.5-1.5v6.8h5.4v3.4h-5.4v15.1c0 .3 0 .6.1 1 0 .4.1.7.4 1.1.2.4.5.6 1 .8.4.3 1 .4 1.8.4 1 0 1.7-.1 2.2-.2V37c-.9.2-2.1.3-3.8.3-2.1 0-3.6-.4-4.6-1.2-1-.8-1.5-2.2-1.5-4.2V15.1h-4.3v-3.4zM737.6 25.2c-.1 2.6.5 4.8 1.7 6.5 1.1 1.7 2.9 2.6 5.3 2.6 1.5 0 2.8-.4 3.9-1.3 1-.8 1.6-2.2 1.8-4h4.6c0 .6-.2 1.4-.4 2.3-.3 1-.8 2-1.7 3-.2.3-.6.6-1 1-.5.4-1 .7-1.7 1.1-.7.4-1.5.6-2.4.8-.9.3-2 .4-3.3.4-7.6-.2-11.3-4.5-11.3-12.9 0-2.5.3-4.8 1-6.8s2-3.7 3.8-5.1c1.2-.8 2.4-1.3 3.7-1.6 1.3-.2 2.2-.3 3-.3 2.7 0 4.8.6 6.3 1.6s2.5 2.3 3.1 3.9c.6 1.5 1 3.1 1.1 4.6.1 1.6.1 2.9 0 4h-17.5m12.9-3v-1.1c0-.4 0-.8-.1-1.2-.1-.9-.4-1.7-.8-2.5s-1-1.5-1.8-2c-.9-.5-2-.8-3.4-.8-.8 0-1.5.1-2.3.3-.8.2-1.5.7-2.2 1.3-.7.6-1.2 1.3-1.6 2.3-.4 1-.7 2.2-.8 3.6h13zM765.3 29.5c0 .5.1 1 .2 1.4.1.5.4 1 .8 1.5s.9.8 1.6 1.1c.7.3 1.6.5 2.7.5 1 0 1.8-.1 2.5-.3.7-.2 1.3-.6 1.7-1.2.5-.7.8-1.5.8-2.4 0-1.2-.4-2-1.3-2.5s-2.2-.9-4.1-1.2c-1.3-.3-2.4-.6-3.6-1-1.1-.3-2.1-.8-3-1.3-.9-.5-1.5-1.2-2-2.1-.5-.8-.8-1.9-.8-3.2 0-2.4.9-4.2 2.6-5.6 1.7-1.3 4-2 6.8-2.1 1.6 0 3.3.3 5 .8 1.7.6 2.9 1.6 3.7 3.1.4 1.4.6 2.6.6 3.7h-4.6c0-1.8-.6-3-1.7-3.5-1.1-.4-2.1-.6-3.1-.6h-1c-.5 0-1.1.2-1.7.4-.6.2-1.1.5-1.5 1.1-.5.5-.7 1.2-.7 2.1 0 1.1.5 1.9 1.3 2.3.7.4 1.5.7 2.1.9 3.3.7 5.6 1.3 6.9 1.8 1.3.4 2.2 1 2.8 1.7.7.7 1.1 1.4 1.4 2.2.3.8.4 1.6.4 2.5 0 1.4-.3 2.7-.9 3.8-.6 1.1-1.4 2-2.4 2.6-1.1.6-2.2 1-3.4 1.3-1.2.3-2.5.4-3.8.4-2.5 0-4.7-.6-6.6-1.8-1.8-1.2-2.8-3.3-2.9-6.3h5.2zM467.7 50.8h21.9V55h-17.1v11.3h16.3v4.2h-16.3v12.1H490v4.3h-22.3zM499 64.7l-.1-2.9h4.6v4.1c.2-.3.4-.8.7-1.2.3-.5.8-1 1.3-1.5.6-.5 1.4-1 2.3-1.3.9-.3 2-.5 3.4-.5.6 0 1.4.1 2.4.2.9.2 1.9.5 2.9 1.1 1 .6 1.8 1.4 2.5 2.5.6 1.2 1 2.7 1 4.7V87h-4.6V71c0-.9-.1-1.7-.2-2.4-.2-.7-.5-1.3-1.1-1.9-1.2-1.1-2.6-1.7-4.3-1.7-1.7 0-3.1.6-4.3 1.8-1.3 1.2-2 3.1-2 5.7V87H499V64.7zM524.6 61.8h5.1l7.7 19.9 7.6-19.9h5l-10.6 25.1h-4.6zM555.7 50.9h5.5V56h-5.5v-5.1m.5 10.9h4.6v25.1h-4.6V61.8zM570.3 67c0-1.8-.1-3.5-.3-5.1h4.6l.1 4.9c.5-1.8 1.4-3 2.5-3.7 1.1-.7 2.2-1.2 3.3-1.3 1.4-.2 2.4-.2 3.1-.1v4.6c-.2-.1-.5-.2-.9-.2h-1.3c-1.3 0-2.4.2-3.3.5-.9.4-1.5.9-2 1.6-.9 1.4-1.4 3.2-1.3 5.4v13.3h-4.6V67zM587.6 74.7c0-1.6.2-3.2.6-4.8.4-1.6 1.1-3 2-4.4 1-1.3 2.2-2.4 3.8-3.2 1.6-.8 3.6-1.2 5.9-1.2 2.4 0 4.5.4 6.1 1.3 1.5.9 2.7 2 3.6 3.3.9 1.3 1.5 2.8 1.8 4.3.2.8.3 1.5.4 2.2v2.2c0 3.7-1 6.9-3 9.5-2 2.6-5.1 4-9.3 4-4-.1-7-1.4-9-3.9-1.9-2.5-2.9-5.6-2.9-9.3m4.8-.3c0 2.7.6 5 1.8 6.9 1.2 2 3 3 5.6 3.1.9 0 1.8-.2 2.7-.5.8-.3 1.6-.9 2.3-1.7.7-.8 1.3-1.9 1.8-3.2.4-1.3.6-2.9.6-4.7-.1-6.4-2.5-9.6-7.1-9.6-.7 0-1.5.1-2.4.3-.8.3-1.7.8-2.5 1.6-.8.7-1.4 1.7-1.9 3-.6 1.1-.9 2.8-.9 4.8zM620.2 64.7l-.1-2.9h4.6v4.1c.2-.3.4-.8.7-1.2.3-.5.8-1 1.3-1.5.6-.5 1.4-1 2.3-1.3.9-.3 2-.5 3.4-.5.6 0 1.4.1 2.4.2.9.2 1.9.5 2.9 1.1 1 .6 1.8 1.4 2.5 2.5.6 1.2 1 2.7 1 4.7V87h-4.6V71c0-.9-.1-1.7-.2-2.4-.2-.7-.5-1.3-1.1-1.9-1.2-1.1-2.6-1.7-4.3-1.7-1.7 0-3.1.6-4.3 1.8-1.3 1.2-2 3.1-2 5.7V87h-4.6V64.7zM650 65.1l-.1-3.3h4.6v3.6c1.2-1.9 2.6-3.2 4.1-3.7 1.5-.4 2.7-.6 3.8-.6 1.4 0 2.6.2 3.6.5.9.3 1.7.7 2.3 1.1 1.1 1 1.9 2 2.3 3.1.2-.4.5-.8 1-1.3.4-.5.9-1 1.5-1.6.6-.5 1.5-.9 2.5-1.3 1-.3 2.2-.5 3.5-.5.9 0 1.9.1 3 .3 1 .2 2 .7 3 1.3 1 .6 1.7 1.5 2.3 2.7.6 1.2.9 2.7.9 4.6v16.9h-4.6V70.7c0-1.1-.1-2-.2-2.5-.1-.6-.3-1-.6-1.3-.4-.6-1-1.2-1.8-1.6-.8-.4-1.8-.6-3.1-.6-1.5 0-2.7.4-3.6 1-.4.3-.8.5-1.1.9l-.8.8c-.5.8-.8 1.8-1 2.8-.1 1.1-.2 2-.1 2.6v14.1h-4.6V70.2c0-1.6-.5-2.9-1.4-4-.9-1-2.3-1.5-4.2-1.5-1.6 0-2.9.4-3.8 1.1-.9.7-1.5 1.2-1.8 1.7-.5.7-.8 1.5-.9 2.5-.1.9-.2 1.8-.2 2.6v14.3H650V65.1zM700.5 75.2c-.1 2.6.5 4.8 1.7 6.5 1.1 1.7 2.9 2.6 5.3 2.6 1.5 0 2.8-.4 3.9-1.3 1-.8 1.6-2.2 1.8-4h4.6c0 .6-.2 1.4-.4 2.3-.3 1-.8 2-1.7 3-.2.3-.6.6-1 1-.5.4-1 .7-1.7 1.1-.7.4-1.5.6-2.4.8-.9.3-2 .4-3.3.4-7.6-.2-11.3-4.5-11.3-12.9 0-2.5.3-4.8 1-6.8s2-3.7 3.8-5.1c1.2-.8 2.4-1.3 3.7-1.6 1.3-.2 2.2-.3 3-.3 2.7 0 4.8.6 6.3 1.6s2.5 2.3 3.1 3.9c.6 1.5 1 3.1 1.1 4.6.1 1.6.1 2.9 0 4h-17.5m12.8-3v-1.1c0-.4 0-.8-.1-1.2-.1-.9-.4-1.7-.8-2.5s-1-1.5-1.8-2c-.9-.5-2-.8-3.4-.8-.8 0-1.5.1-2.3.3-.8.2-1.5.7-2.2 1.3-.7.6-1.2 1.3-1.6 2.3-.4 1-.7 2.2-.8 3.6h13zM725.7 64.7l-.1-2.9h4.6v4.1c.2-.3.4-.8.7-1.2.3-.5.8-1 1.3-1.5.6-.5 1.4-1 2.3-1.3.9-.3 2-.5 3.4-.5.6 0 1.4.1 2.4.2.9.2 1.9.5 2.9 1.1 1 .6 1.8 1.4 2.5 2.5.6 1.2 1 2.7 1 4.7V87h-4.6V71c0-.9-.1-1.7-.2-2.4-.2-.7-.5-1.3-1.1-1.9-1.2-1.1-2.6-1.7-4.3-1.7-1.7 0-3.1.6-4.3 1.8-1.3 1.2-2 3.1-2 5.7V87h-4.6V64.7zM752.3 61.7h4.3v-5.2l4.5-1.5v6.8h5.4v3.4h-5.4v15.1c0 .3 0 .6.1 1 0 .4.1.7.4 1.1.2.4.5.6 1 .8.4.3 1 .4 1.8.4 1 0 1.7-.1 2.2-.2V87c-.9.2-2.1.3-3.8.3-2.1 0-3.6-.4-4.6-1.2-1-.8-1.5-2.2-1.5-4.2V65.1h-4.3v-3.4zM787.6 86.9c-.3-1.2-.5-2.5-.4-3.7-.5 1-1.1 1.8-1.7 2.4-.7.6-1.4 1.1-2 1.4-1.4.5-2.7.8-3.7.8-2.8 0-4.9-.8-6.4-2.2-1.5-1.4-2.2-3.1-2.2-5.2 0-1 .2-2.3.8-3.7.6-1.4 1.7-2.6 3.5-3.7 1.4-.7 2.9-1.2 4.5-1.5 1.6-.1 2.9-.2 3.9-.2s2.1 0 3.3.1c.1-2.9-.2-4.8-.9-5.6-.5-.6-1.1-1.1-1.9-1.3-.8-.2-1.6-.4-2.3-.4-1.1 0-2 .2-2.6.5-.7.3-1.2.7-1.5 1.2-.3.5-.5.9-.6 1.4-.1.5-.2.9-.2 1.2h-4.6c.1-.7.2-1.4.4-2.3.2-.8.6-1.6 1.3-2.5.5-.6 1-1 1.7-1.3.6-.3 1.3-.6 2-.8 1.5-.4 2.8-.6 4.2-.6 1.8 0 3.6.3 5.2.9 1.6.6 2.8 1.6 3.4 2.9.4.7.6 1.4.7 2 .1.6.1 1.2.1 1.8l-.2 12c0 1 .1 3.1.4 6.3h-4.2m-.5-12.1c-.7-.1-1.6-.1-2.6-.1h-2.1c-1 .1-2 .3-3 .6s-1.9.8-2.6 1.5c-.8.7-1.2 1.7-1.2 3 0 .4.1.8.2 1.3s.4 1 .8 1.5.9.8 1.6 1.1c.7.3 1.5.5 2.5.5 2.3 0 4.1-.9 5.2-2.7.5-.8.8-1.7 1-2.7.1-.9.2-2.2.2-4zM800.7 50.9h4.6V87h-4.6zM828.4 50.8h11.7c2.1 0 3.9.1 5.5.4.8.2 1.5.4 2.2.9.7.4 1.3.9 1.8 1.6 1.7 1.9 2.6 4.2 2.6 7 0 2.7-.9 5.1-2.8 7.1-.8.9-2 1.7-3.6 2.2-1.6.6-3.9.9-6.9.9h-5.7V87h-4.8V50.8m4.8 15.9h5.8c.8 0 1.7-.1 2.6-.2.9-.1 1.8-.3 2.6-.7.8-.4 1.5-1 2-1.9.5-.8.8-2 .8-3.4s-.2-2.5-.7-3.3c-.5-.8-1.1-1.3-1.9-1.7-1.6-.5-3.1-.8-4.5-.7h-6.8v11.9zM858.1 67c0-1.8-.1-3.5-.3-5.1h4.6l.1 4.9c.5-1.8 1.4-3 2.5-3.7 1.1-.7 2.2-1.2 3.3-1.3 1.4-.2 2.4-.2 3.1-.1v4.6c-.2-.1-.5-.2-.9-.2h-1.3c-1.3 0-2.4.2-3.3.5-.9.4-1.5.9-2 1.6-.9 1.4-1.4 3.2-1.3 5.4v13.3H858V67zM875.5 74.7c0-1.6.2-3.2.6-4.8.4-1.6 1.1-3 2-4.4 1-1.3 2.2-2.4 3.8-3.2 1.6-.8 3.6-1.2 5.9-1.2 2.4 0 4.5.4 6.1 1.3 1.5.9 2.7 2 3.6 3.3.9 1.3 1.5 2.8 1.8 4.3.2.8.3 1.5.4 2.2v2.2c0 3.7-1 6.9-3 9.5-2 2.6-5.1 4-9.3 4-4-.1-7-1.4-9-3.9-1.9-2.5-2.9-5.6-2.9-9.3m4.8-.3c0 2.7.6 5 1.8 6.9 1.2 2 3 3 5.6 3.1.9 0 1.8-.2 2.7-.5.8-.3 1.6-.9 2.3-1.7.7-.8 1.3-1.9 1.8-3.2.4-1.3.6-2.9.6-4.7-.1-6.4-2.5-9.6-7.1-9.6-.7 0-1.5.1-2.4.3-.8.3-1.7.8-2.5 1.6-.8.7-1.4 1.7-1.9 3-.7 1.1-.9 2.8-.9 4.8zM904.1 61.7h4.3v-5.2l4.5-1.5v6.8h5.4v3.4h-5.4v15.1c0 .3 0 .6.1 1 0 .4.1.7.4 1.1.2.4.5.6 1 .8.4.3 1 .4 1.8.4 1 0 1.7-.1 2.2-.2V87c-.9.2-2.1.3-3.8.3-2.1 0-3.6-.4-4.6-1.2-1-.8-1.5-2.2-1.5-4.2V65.1h-4.3v-3.4zM927.2 75.2c-.1 2.6.5 4.8 1.7 6.5 1.1 1.7 2.9 2.6 5.3 2.6 1.5 0 2.8-.4 3.9-1.3 1-.8 1.6-2.2 1.8-4h4.6c0 .6-.2 1.4-.4 2.3-.3 1-.8 2-1.7 3-.2.3-.6.6-1 1-.5.4-1 .7-1.7 1.1-.7.4-1.5.6-2.4.8-.9.3-2 .4-3.3.4-7.6-.2-11.3-4.5-11.3-12.9 0-2.5.3-4.8 1-6.8s2-3.7 3.8-5.1c1.2-.8 2.4-1.3 3.7-1.6 1.3-.2 2.2-.3 3-.3 2.7 0 4.8.6 6.3 1.6s2.5 2.3 3.1 3.9c.6 1.5 1 3.1 1.1 4.6.1 1.6.1 2.9 0 4h-17.5m12.9-3v-1.1c0-.4 0-.8-.1-1.2-.1-.9-.4-1.7-.8-2.5s-1-1.5-1.8-2c-.9-.5-2-.8-3.4-.8-.8 0-1.5.1-2.3.3-.8.2-1.5.7-2.2 1.3-.7.6-1.2 1.3-1.6 2.3-.4 1-.7 2.2-.8 3.6h13zM966.1 69.8c0-.3 0-.8-.1-1.4-.1-.6-.3-1.1-.6-1.8-.2-.6-.7-1.2-1.4-1.6-.7-.4-1.6-.6-2.7-.6-1.5 0-2.7.4-3.5 1.2-.9.8-1.5 1.7-1.9 2.8-.4 1.1-.6 2.2-.7 3.2-.1 1.1-.2 1.8-.1 2.4 0 1.3.1 2.5.3 3.7.2 1.2.5 2.3.9 3.3.8 2 2.4 3 4.8 3.1 1.9 0 3.3-.7 4.1-1.9.8-1.1 1.2-2.3 1.2-3.6h4.6c-.2 2.5-1.1 4.6-2.7 6.3-1.7 1.8-4.1 2.7-7.1 2.7-.9 0-2.1-.2-3.6-.6-.7-.2-1.4-.6-2.2-1-.8-.4-1.5-1-2.2-1.7-.7-.9-1.4-2.1-2-3.6-.6-1.5-.9-3.5-.9-6.1 0-2.6.4-4.8 1.1-6.6.7-1.7 1.6-3.1 2.7-4.2 1.1-1 2.3-1.8 3.6-2.2 1.3-.4 2.5-.6 3.7-.6h1.6c.6.1 1.3.2 1.9.4.7.2 1.4.5 2.1 1 .7.4 1.3 1 1.8 1.7.9 1.1 1.4 2.1 1.7 3.1.2 1 .3 1.8.3 2.6h-4.7zM973.6 61.7h4.3v-5.2l4.5-1.5v6.8h5.4v3.4h-5.4v15.1c0 .3 0 .6.1 1 0 .4.1.7.4 1.1.2.4.5.6 1 .8.4.3 1 .4 1.8.4 1 0 1.7-.1 2.2-.2V87c-.9.2-2.1.3-3.8.3-2.1 0-3.6-.4-4.6-1.2-1-.8-1.5-2.2-1.5-4.2V65.1h-4.3v-3.4zM993.5 50.9h5.5V56h-5.5v-5.1m.5 10.9h4.6v25.1H994V61.8zM1006.1 74.7c0-1.6.2-3.2.6-4.8.4-1.6 1.1-3 2-4.4 1-1.3 2.2-2.4 3.8-3.2 1.6-.8 3.6-1.2 5.9-1.2 2.4 0 4.5.4 6.1 1.3 1.5.9 2.7 2 3.6 3.3.9 1.3 1.5 2.8 1.8 4.3.2.8.3 1.5.4 2.2v2.2c0 3.7-1 6.9-3 9.5-2 2.6-5.1 4-9.3 4-4-.1-7-1.4-9-3.9-1.9-2.5-2.9-5.6-2.9-9.3m4.7-.3c0 2.7.6 5 1.8 6.9 1.2 2 3 3 5.6 3.1.9 0 1.8-.2 2.7-.5.8-.3 1.6-.9 2.3-1.7.7-.8 1.3-1.9 1.8-3.2.4-1.3.6-2.9.6-4.7-.1-6.4-2.5-9.6-7.1-9.6-.7 0-1.5.1-2.4.3-.8.3-1.7.8-2.5 1.6-.8.7-1.4 1.7-1.9 3-.6 1.1-.9 2.8-.9 4.8zM1038.6 64.7l-.1-2.9h4.6v4.1c.2-.3.4-.8.7-1.2.3-.5.8-1 1.3-1.5.6-.5 1.4-1 2.3-1.3.9-.3 2-.5 3.4-.5.6 0 1.4.1 2.4.2.9.2 1.9.5 2.9 1.1 1 .6 1.8 1.4 2.5 2.5.6 1.2 1 2.7 1 4.7V87h-4.6V71c0-.9-.1-1.7-.2-2.4-.2-.7-.5-1.3-1.1-1.9-1.2-1.1-2.6-1.7-4.3-1.7-1.7 0-3.1.6-4.3 1.8-1.3 1.2-2 3.1-2 5.7V87h-4.6V64.7zM479.1 100.8h5.2l14.1 36.1h-5.3l-3.8-9.4h-16.2l-3.8 9.4h-5l14.8-36.1m-4.4 22.7H488l-6.5-17.8-6.8 17.8zM508.7 138.8c.1.7.2 1.4.4 1.9.2.6.5 1.1.9 1.6.8.9 2.3 1.4 4.4 1.5 1.6 0 2.8-.3 3.7-.9.9-.6 1.5-1.4 1.9-2.4.4-1.1.6-2.3.7-3.7.1-1.4.1-2.9.1-4.6-.5.9-1.1 1.7-1.8 2.3-.7.6-1.5 1-2.3 1.3-1.7.4-3 .6-3.9.6-1.2 0-2.4-.2-3.8-.6-1.4-.4-2.6-1.2-3.7-2.5-1-1.3-1.7-2.8-2.1-4.4-.4-1.6-.6-3.2-.6-4.8 0-4.3 1.1-7.4 3.2-9.5 2-2.1 4.6-3.1 7.6-3.1 1.3 0 2.3.1 3.2.4.9.3 1.6.6 2.1 1 .6.4 1.1.8 1.5 1.2l.9 1.2v-3.4h4.4l-.1 4.5v15.7c0 2.9-.1 5.2-.2 6.7-.2 1.6-.5 2.8-1 3.7-1.1 1.9-2.6 3.2-4.6 3.7-1.9.6-3.8.8-5.6.8-2.4 0-4.3-.3-5.6-.8-1.4-.5-2.4-1.2-3-2-.6-.8-1-1.7-1.2-2.7-.2-.9-.3-1.8-.4-2.7h4.9m5.3-5.8c1.4 0 2.5-.2 3.3-.7.8-.5 1.5-1.1 2-1.8.5-.6.9-1.4 1.2-2.5.3-1 .4-2.6.4-4.8 0-1.6-.2-2.9-.4-3.9-.3-1-.8-1.8-1.4-2.4-1.3-1.4-3-2.2-5.2-2.2-1.4 0-2.5.3-3.4 1-.9.7-1.6 1.5-2 2.4-.4 1-.7 2-.9 3-.2 1-.2 2-.2 2.8 0 1 .1 1.9.3 2.9.2 1.1.5 2.1 1 3 .5.9 1.2 1.6 2 2.2.8.7 1.9 1 3.3 1zM537.6 125.2c-.1 2.6.5 4.8 1.7 6.5 1.1 1.7 2.9 2.6 5.3 2.6 1.5 0 2.8-.4 3.9-1.3 1-.8 1.6-2.2 1.8-4h4.6c0 .6-.2 1.4-.4 2.3-.3 1-.8 2-1.7 3-.2.3-.6.6-1 1-.5.4-1 .7-1.7 1.1-.7.4-1.5.6-2.4.8-.9.3-2 .4-3.3.4-7.6-.2-11.3-4.5-11.3-12.9 0-2.5.3-4.8 1-6.8s2-3.7 3.8-5.1c1.2-.8 2.4-1.3 3.7-1.6 1.3-.2 2.2-.3 3-.3 2.7 0 4.8.6 6.3 1.6s2.5 2.3 3.1 3.9c.6 1.5 1 3.1 1.1 4.6.1 1.6.1 2.9 0 4h-17.5m12.9-3v-1.1c0-.4 0-.8-.1-1.2-.1-.9-.4-1.7-.8-2.5s-1-1.5-1.8-2.1c-.9-.5-2-.8-3.4-.8-.8 0-1.5.1-2.3.3-.8.2-1.5.7-2.2 1.3-.7.6-1.2 1.3-1.6 2.3-.4 1-.7 2.2-.8 3.7h13zM562.9 114.7l-.1-2.9h4.6v4.1c.2-.3.4-.8.7-1.2.3-.5.8-1 1.3-1.5.6-.5 1.4-1 2.3-1.3.9-.3 2-.5 3.4-.5.6 0 1.4.1 2.4.2.9.2 1.9.5 2.9 1.1 1 .6 1.8 1.4 2.5 2.5.6 1.2 1 2.7 1 4.7V137h-4.6v-16c0-.9-.1-1.7-.2-2.4-.2-.7-.5-1.3-1.1-1.9-1.2-1.1-2.6-1.7-4.3-1.7-1.7 0-3.1.6-4.3 1.8-1.3 1.2-2 3.1-2 5.7V137h-4.6v-22.3zM607 119.8c0-.3 0-.8-.1-1.4-.1-.6-.3-1.1-.6-1.8-.2-.6-.7-1.2-1.4-1.6-.7-.4-1.6-.6-2.7-.6-1.5 0-2.7.4-3.5 1.2-.9.8-1.5 1.7-1.9 2.8-.4 1.1-.6 2.2-.7 3.2-.1 1.1-.2 1.8-.1 2.4 0 1.3.1 2.5.3 3.7.2 1.2.5 2.3.9 3.3.8 2 2.4 3 4.8 3.1 1.9 0 3.3-.7 4.1-1.9.8-1.1 1.2-2.3 1.2-3.6h4.6c-.2 2.5-1.1 4.6-2.7 6.3-1.7 1.8-4.1 2.7-7.1 2.7-.9 0-2.1-.2-3.6-.6-.7-.2-1.4-.6-2.2-1-.8-.4-1.5-1-2.2-1.7-.7-.9-1.4-2.1-2-3.6-.6-1.5-.9-3.5-.9-6.1 0-2.6.4-4.8 1.1-6.6.7-1.7 1.6-3.1 2.7-4.2 1.1-1 2.3-1.8 3.6-2.2 1.3-.4 2.5-.6 3.7-.6h1.6c.6.1 1.3.2 1.9.4.7.2 1.4.5 2.1 1 .7.4 1.3 1 1.8 1.7.9 1.1 1.4 2.1 1.7 3.1.2 1 .3 1.8.3 2.6H607zM629.1 137.1l-3.4 9.3H621l3.8-9.6-10.3-25h5.2l7.6 19.8 7.7-19.8h5z"/>
                </svg>
              </span>
            </a>
            <button class="usa-menu-btn usa-button l-header__menu-button">Menu</button>
          </div>
          <div class="l-header__search">
            <form class="usa-search usa-search--small usa-search--epa" method="get" action="https://search.epa.gov/epasearch">
              <div role="search">
                <label class="usa-sr-only" for="search-box">Search</label>
                <input class="usa-input" id="search-box" type="search" name="querytext" placeholder="Search EPA.gov">
                <!-- button class="usa-button" type="submit" --> <!-- type="submit" - removed for now to allow other unrendered buttons to render when triggered in RShiny app -->
                <!-- see: https://github.com/rstudio/shiny/issues/2922 -->
                <button class="usa-button usa-search__submit" style="height:2rem;margin:0;padding:0;padding-left:1rem;padding-right:1rem;border-top-left-radius: 0;border-bottom-left-radius: 0;">
                  <span class="usa-sr-only">Search</span>
                </button>
                <input type="hidden" name="areaname" value="">
                <input type="hidden" name="areacontacts" value="">
                <input type="hidden" name="areasearchurl" value="">
                <input type="hidden" name="typeofsearch" value="epa">
                <input type="hidden" name="result_template" value="">
              </div>
            </form>
          </div>
        </div>
      </div>
      <div class="l-header__nav">
        <nav class="usa-nav usa-nav--epa" role="navigation" aria-label="EPA header navigation">
          <div class="usa-nav__inner">
            <button class="usa-nav__close" aria-label="Close">
              <svg class="icon icon--nav-close" aria-hidden="true" role="img">
                <title>Primary navigation</title>
                <use xlink:href="https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#close"></use>
              </svg> </button>
            <div class="usa-nav__menu">
               <ul class="menu menu--main">
                <li class="menu__item"><a href="https://www.epa.gov/environmental-topics" class="menu__link">Environmental Topics</a></li>
                <li class="menu__item"><a href="https://www.epa.gov/laws-regulations" class="menu__link" >Laws &amp; Regulations</a></li>
                <li class="menu__item"><a href="https://www.epa.gov/report-violation" class="menu__link" >Report a Violation</a></li>
                <li class="menu__item"><a href="https://www.epa.gov/aboutepa" class="menu__link" >About EPA</a></li>
              </ul>
            </div>
          </div>
        </nav>
      </div>
    </header>
    <main id="main" class="main" role="main" tabindex="-1">'
    ),
    
    # Individual Page Header
    htmltools::HTML(
      '<div class="l-page  has-footer">
      <div class="l-constrain">
        <div class="l-page__header">
          <div class="l-page__header-first">
            <div class="web-area-title"></div>
          </div>
          <div class="l-page__header-last">
            <a href="#" class="header-link">Contact Us</a>
          </div>
        </div>
        <article class="article">'
    ),
    
    
    
    # Insert your UI code here
    ## Begin example UI code
    
    
    
    shinybusy::add_busy_spinner(spin = "fading-circle", position = c("top-right"),height = "50px",width = "50px"),
    
    # Enable Shinyjs -----
    shinyjs::useShinyjs(),
    
    # Shiny app title -----
    title = "Final Ecosystem Goods and Services (FEGS) Document Reader",
    
    # Display the app title as the tab label in a browser window -----
    shinytitle::use_shiny_title(),
    
    # Title and subtitle displayed at the top of the app page -----
    shiny::titlePanel(
      
      # App title
      shiny::h1("Final Ecosystem Goods and Services (FEGS) Document Reader",
                
                # App subtitle/description
                shiny::h4("The FEGS Document Reader is designed to help identify and prioritize ecosystem services attributes and the beneficiaries who use or care about them when existing written information is available, based on a search for keyword terms in pdf documents. Though not intended to substitute for thoughtful stakeholder engagement, the FEGS Document Reader can serve as a starting point for more efficient or targeted stakeholder engagement to refine ecosystem services priorities. The application can also facilitate a consistent approach for comparing across documents using a structured set of keywords.",
                         style = "font-size: 12pt;"),
                
                ### Add a running clock of session time (primarily to force refresh and prevent timing out)
                ##p("Elapsed session time: ", textOutput("clock", inline = TRUE))
                shiny::p(shiny::textOutput("clock", inline = TRUE))
                
      )  # Close h1
      
    ),  # Close title panel
    
    
    # Set CSS formatting -----
    htmltools::tags$head(
      
      htmltools::tags$style(
        
        htmltools::HTML(
          
          # Header 1 (h1) -----
          ".title-panel h1 {
          font-size: 28pt;
          margin: 0;
          margin-right: 12px;
          margin-top: 10px;
          line-height: 1.2;
        }",
          
          # Header 2 (h2) -----
          ".title-panel h4 {
          font-size: 18pt;
          margin-top: 20px;
          margin-bottom: 10px;
        }",
          
          # Progress message box -----
          ".shiny-progress .progress-text{
          height: 50px;
          width: 175px;
        }",
          
          # Progress message text -----
          ".shiny-progress .progress-text .progress-message {
          font-size: 14pt;
          font-weight: bold;
          position: right;
        }",
          
          # Progress details text -----
          ".shiny-progress .progress-text .progress-message {
          font-size: 12pt;
          position: right;
        }",
          
          # Tab set titles text -----
          ".nav-tabs {
          font-size: 14pt;
        }",
          
          # Upload file placeholder text -----
          "input::placeholder {
          font-size: 10pt;
        }",
          
          # Upload file progress bar -----
          ".progress-bar {
          background-color: green;
        }",
          
          # Drop-down menu text -----
          ".selectize-dropdown {
          font-size: 11pt;
        }",
          
          # Ecosystem plot title text -----
          "#eco_title {
          font-size: 15pt;
          font-style: bold;
        }",
          
          # Beneficiary plot title text -----
          "#ben_title {
          font-size: 15pt;
          font-style: bold;
        }",
          
          # Services plot title text -----
          "#fegs_title {
          font-size: 15pt;
          font-style: bold;
        }",
          
          # Word Cloud plot title text -----
          "#wordcloud_title {
          font-size: 15pt;
          font-style: bold;
        }",
          
          
          # Ecosystem plot description text -----
          "#eco_description {
          font-size: 11pt;
          font-style: bold;
        }",
          
          # Beneficiary plot description text -----
          "#ben_description {
          font-size: 11pt;
          font-style: bold;
        }",
          
          # Services plot description text -----
          "#fegs_description {
          font-size: 11pt;
          font-style: bold;
        }",
          
          # Word Cloud plot description text -----
          "#wordcloud_description {
          font-size: 11pt;
          font-style: bold;
        }",
          
          # Beneficiary profile figures legend -----
          ".ben_leg {
          width: 100%;
          float: right;
        }",
          
          # FEGS profile figures legend -----
          ".fegs_leg {
          width: 100%;
          float: right;
        }"
          
        )  # Close HTML
        
      ),  # Close tags$style
      
    ),  # Close tages$head
    
    
    # Set the app layout - sidebar and main panel of tabs -----
    shiny::sidebarLayout(
      
      # Sidebar: Inputs and options ----
      shiny::sidebarPanel(
        
        # Input: docs_upload ----
        # Clicking this button will allow the user to select one or multiple documents (PDFs) to upload
        htmltools::div(
          
          shiny::fileInput("docs_upload", "Upload Document(s)",
                           # Allow multiple files to be uploaded simultaneously
                           multiple = TRUE,
                           # Accept only PDF documents
                           # This validation doesn't appear to work in R, but should work in a browser
                           accept = ".pdf"),
          
          # Style the "Upload Document(s)" text
          style = "font-size: 12pt;"),
        
        # Action button: find_matches_btn -----
        # Clicking this button will find keyword matches in the uploaded document(s) and filter them for triplets
        # Start the app with this button disabled - shouldn't be available until document(s) uploaded
        shinyjs::disabled(shiny::actionButton("find_matches_btn", "Find Matches",
                                              # Style the button
                                              style = "margin-top: 5px; margin-bottom: 50px; font-size: 11pt;")),
        
        # Action button: cancel_match_btn -----
        # Clicking this button will cancel the process of finding keyword matches
        # Start the app with this button disabled - should only be available when the keyword matching process is running
        shinyjs::disabled(shiny::actionButton("cancel_btn", "Cancel",
                                              # Style the button
                                              style = "margin-top: 5px; margin-bottom: 50px; font-size: 11pt;")),
        
        # Add break so next set of buttons are on a different line
        shiny::br(),
        
        # Radio buttons: plot_type -----
        # The selection will determine what type of figure is displayed in the beneficiary and ecosystem services profile tabs
        htmltools::div(
          
          # Start the app with these buttons disabled - shouldn't be available until figures have been created
          shinyjs::disabled(shiny::radioButtons("plot_type", "Choose figure type:",
                                                # Set the button options
                                                c("Bar Chart" = "bar",
                                                  "Pie Chart" = "pie"))),
          
          # Style the "Choose figure type:" text
          style = "font-size: 12pt;"),
        
        # Drop-down menu: eco_dd -----
        htmltools::div(
          
          # Start the app with this drop-down menu disabled - shouldn't be available until figures have been created
          shinyjs::disabled(shiny::selectInput("eco_dd", "Display which ecosystem?",
                                               # List of ecosystem choices displayed in the drop down menu
                                               # These options are taken from mean_ffreq() and show only ecosystems with matches
                                               choices = "")),
          
          # Style the "Display which ecosystem?" text
          style = "font-size: 12pt;"),
        
        # Radio buttons: file_type -----
        # The selection will determine the file type of the results export
        htmltools::div(
          
          # Start the app with these buttons disabled - shouldn't be available until triplets have been found and frequencies calculated
          shinyjs::disabled(shiny::radioButtons("file_type", "Export results as:",
                                                # Set the button options
                                                c(".xlsx" = "xl",
                                                  ".fegs" = "fegs"))),
          
          # Style the "Export results as:" text
          style = "font-size: 12pt; margin-top: 50px;"),
        
        
        # Download button: export_res_btn -----
        # Clicking this button will allow the user to download the results of the keyword matching and frequency calculation as an Excel file
        # Start the app with this button disabled - shouldn't be available until triplets have been found and frequencies calculated
        shinyjs::disabled(shiny::downloadButton("export_res_btn", "Export",
                                                # Style the button
                                                style = "margin-bottom: 25px; font-size: 11pt;")),
        
        # Add a break so this button is on its own line
        shiny::br(),
        
        # Radio buttons: eco_fig_export -----
        # The selection will determine which figures are exported: all ecosystems or a selection of a few
        
        htmltools::div(
          
          # Start the app with this button disabled - shouldn't be available until figures have been created
          shinyjs::disabled(shiny::radioButtons("eco_fig_export", "Export which figures:",
                                                # Set the button options
                                                c("All Ecosystems" = "all",
                                                  "Select from list" = "select"))),
          
          # Style the "Export which figures:" text
          style = "font-size: 12pt;"),
        
        # Checkbox input: eco_fig_sel -----
        # Selecting an ecosystem from the list will add it to the zip file of exported figures
        shiny::conditionalPanel(
          
          # Display the panel when the "Select from list" radio button is selected
          condition = "input.eco_fig_export == `select`",
          
          htmltools::div(
            
            shiny::checkboxGroupInput("eco_fig_sel", "Ecosystems available for export:",
                                      # Display ecosystem choices
                                      choices = ""),
            
            # Style the text
            style = "font-size: 12pt;")
          
        ),  # Close conditional panel
        
        # Download button: export_fig_btn -----
        # Clicking this button will allow the user to download the specified figures as a .zip file
        # Start the app with this button disabled - shouldn't be available until figure download options have been selected
        shinyjs::disabled(shiny::downloadButton("export_fig_btn", "Export",
                                                # Style the button
                                                style = "font-size: 11pt;")),
        
        # Set the width of the sidebar panel -----
        width = 3,
        
      ),  # Close sidebar panel
      
      
      # Main panel: Output display ----
      shiny::mainPanel(
        
        # Create individual tabs for each generated output -----
        shiny::tabsetPanel(
          
          # Tab 1: Instructions & Document(s) -----
          shiny::tabPanel(title = "Documents",
                          
                          # Add a three-column row for the instructions and uploaded document(s) table -----
                          shiny::fluidRow(
                            
                            # Column 1: Instructions -----
                            # This column houses the instructions for using the FEGS Doc Reader
                            shiny::column(7,
                                          
                                          # Add a break to space the text from the top of the tab
                                          shiny::br(),
                                          
                                          # Instructions heading in bold
                                          shiny::strong("Instructions for the FEGS Document Reader:",
                                                        style = "font-size: 14pt;"),
                                          
                                          # Add breaks to space the heading from the remaining text
                                          shiny::br(),
                                          
                                          # Step 1
                                          shiny::p("Step 1: Upload pdf documents",
                                                   style = "font-size: 12pt;"),
                                          
                                          # Step 2
                                          shiny::p("Step 2: Find keyword matches in sentences",
                                                   style = "font-size: 12pt;"),
                                          
                                          # Step 3
                                          shiny::p("Step 3: Create and view figures",
                                                   style = "font-size: 12pt;"),
                                          
                                          # Add a break to space the instructions steps
                                          shiny::br(),
                                          shiny::p("For more information and examples, refer to the User Manual.",style = "font-size: 12pt;"),
                                          shiny::a("User Manual",target="_blank",href="FEGSDR_UserGuide.pdf",style = "font-size: 14pt;")
                                          
                            ),  # Close column 1
                            
                            # Column 2: Uploaded document(s) -----
                            # This column houses the table of uploaded documents
                            shiny::column(4,
                                          
                                          # Add a break to space the text from the top of the tab
                                          shiny::br(),
                                          
                                          # Table title in bold
                                          shiny::strong("Uploaded Document(s):",
                                                        style = "font-size: 14pt;"),
                                          
                                          shiny::p("Click 'Find Matches' to Run (or Re-Run) keyword search after adding or removing documents.",
                                                   style = "font-size: 11pt;"),

                                          # Add a break to space the title from the table
                                          ##shiny::br(),
                                          
                                          # UI output: ui_doc_message -----
                                          # This output will either state that document(s) have not yet been uploaded or it will display the table of uploaded document(s)
                                          shiny::uiOutput("ui_doc_message"),
                                          
                                          # Add a break to space the table from the action button
                                          shiny::br(),
                                          
                                          # Action button: remove_docs_btn -----
                                          # Clicking the button will remove selected rows (document[s]) from the table (keyword search)
                                          # Start the app with this button disabled - should only be available after document(s) have been uploaded
                                          shinyjs::disabled(shiny::actionButton("remove_docs_btn", "Remove selected file(s)",
                                                                                # Set styling
                                                                                style = "margin-bottom: 35px; font-size: 11pt;"))
                                          
                            ),  # Close column 2
                            
                            # Column 3: Empty -----
                            # This column is empty but provides a buffer between the table of uploaded document(s) and the edge of the page
                            shiny::column(1)
                            
                          ),  # Close fluid row
                          
                          # Add breaks to space the instructions/uploaded document(s) table and the output table of triplet matches
                          shiny::br(),
                          
                          shiny::br(),
                          
                          # Table title in bold
                          shiny::strong("Keyword Matches:",
                                        style = "font-size:14pt;"),
                          
                          # Add a break to space the title from the table
                          shiny::br(),
                          
                          # UI output: ui_matches_message -----
                          # This output will either state that keyword matches have not yet been found or it will display the table of triplet matches
                          shiny::uiOutput("ui_matches_message"),
                          
                          # Add breaks to space the triplet matches table and the bottom of the page
                          shiny::br(),
                          shiny::br()
                          
          ),  # Close Instructions & Document(s) tab panel
          
          # Tab 2: Ecosystem profile -----
          shiny::tabPanel(title = "Environment Profile",
                          
                          # Add break to space the figure title from the top of the tab
                          shiny::br(),
                          
                          # Text output: eco_title -----
                          # This figure title will only render/display once the ecosystem frequencies in the uploaded document(s) have been calculated and figures created
                          shiny::textOutput("eco_title"),
                          shiny::textOutput("eco_description"),
                          
                          # Add break to space the title from the figure
                          shiny::br(),
                          
                          # UI output: ui_eco_bars_message -----
                          # This output will either state that figures have not yet been created or it will display the ecosystems found in document(s) figure
                          shiny::uiOutput("ui_eco_bars_message"),
                          
                          # Add breaks to space the ecosystem figure and the bottom of the page
                          shiny::br(),
                          
                          shiny::br()
                          
          ),  # Close Ecosystem profile tab panel
          
          # Tab 3: Ecosystem beneficiary profile -----
          shiny::tabPanel(title = "Beneficiary Profile",
                          
                          # Add break to space the figure title from the top of the tab
                          shiny::br(),
                          
                          # Text output: ben_title -----
                          # This figure title will only render/display once the beneficiary frequencies in the uploaded document(s) have been calculated and figures created
                          shiny::textOutput("ben_title"),
                          shiny::textOutput("ben_description"),
                          
                          # Add break to space the title from the figures
                          shiny::br(),
                          
                          # UI output: ui_ben_figs_message -----
                          # This output will either state that figures have not yet been created or it will display the beneficiary profile figures
                          shiny::uiOutput("ui_ben_figs_message"),
                          
                          # Add breaks to space the beneficiary figure and the bottom of the page
                          shiny::br(),
                          
                          shiny::br()
                          
          ),  # Close Ecosystem benficiary profile tab panel
          
          # Tab 4: Ecosystem services profile -----
          shiny::tabPanel(title = "Attribute Profile",
                          
                          # Add break to space the figure title from the top of the tab
                          shiny::br(),
                          
                          # Text output: fegs_title -----
                          # This figure title will only render/display once the services frequencies in the uploaded document(s) have been calculated and figures created
                          shiny::textOutput("fegs_title"),
                          shiny::textOutput("fegs_description"),
                          
                          # Add break to space the title from the figures
                          shiny::br(),
                          
                          # UI output: ui_fegs_figs_message -----
                          # This output will either state that figures have not yet been created or it will display the services profile figures
                          shiny::uiOutput("ui_fegs_figs_message"),
                          
                          # Add breaks to space the ecosystem services pie charts and the bottom of the page
                          shiny::br(),
                          
                          shiny::br()
                          
          ),  # Close Ecosystem services profile tab panel
          
          # Tab 5: Word Cloud profile -----
          shiny::tabPanel(title = "Word Cloud",
                          
                          # Add break to space the figure title from the top of the tab
                          shiny::br(),
                          
                          # Text output: wordcloud_title -----
                          # This figure title will only render/display once the services frequencies in the uploaded document(s) have been calculated and figures created
                          shiny::textOutput("wordcloud_title"),
                          shiny::textOutput("wordcloud_description"),
                          
                          # Add break to space the title from the figures
                          shiny::br(),
                          
                          # UI output: ui_wordcloud_figs_message -----
                          # This output will either state that figures have not yet been created or it will display the services profile figures
                          shiny::uiOutput("ui_wordcloud_figs_message"),
                          
                          # Add breaks to space the figure and the bottom of the page
                          shiny::br(),
                          shiny::br()
                          
          )  # Word Cloud profile tab panel
          
          
        ),  # Close tab set panel
        
        # Set the width of the main panel -----
        width = 9
        
      )  # Close main panel
      
      
    ),  # Close sidebar layout
    
    # Set the theme of the app -----
    theme = shinythemes::shinytheme("spacelab")

    
    ## End example UI code
    
    # IMPORTANT! For a navbar page, you will need to place the header and footer inside the navbar section (as shown below)  -
    # you will then want to comment out lines 201-213 and lines 254-263
    #   navbarPage(
    #     title = h2("Sample App"),
    #     header = HTML(
    #       '<div class="l-page  has-footer">
    #         <div class="l-constrain">
    #           <div class="l-page__header">
    #             <div class="l-page__header-first">
    #               <div class="web-area-title"></div>
    #             </div>
    #             <div class="l-page__header-last">
    #               <a href="#" class="header-link">Contact Us</a>
    #             </div>
    #           </div>
    #           <article class="article">'
    #     ),
    #     footer = HTML(
    #       '</article>
    # 	        </div>
    #           <div class="l-page__footer">
    #             <div class="l-constrain">
    #               <p><a href="#">Contact Us</a> to ask a question, provide feedback, or report a problem.</p>
    #             </div>
    #           </div>
    #         </div>'
    #     ),
    #     tabPanel("Sample Tab 1"),
    #     tabPanel("Sample Tab 2"),
    #   ),
    
    # Individual Page Footer
    , htmltools::HTML(
      '</article>
    </div>
    <div class="l-page__footer">
      <div class="l-constrain">
        <p><a href="#">Contact Us</a> to ask a question, provide feedback, or report a problem.</p>
      </div>
    </div>
  </div>'
    ),
    
    # Site Footer
    htmltools::HTML(
      '</main>
      <footer class="footer" role="contentinfo">
      <div class="l-constrain">
        <img class="footer__epa-seal" src="https://www.epa.gov/themes/epa_theme/images/epa-seal.svg" alt="United States Environmental Protection Agency" height="100" width="100">
        <div class="footer__content contextual-region">
          <div class="footer__column">
            <h2>Discover.</h2>
            <ul class="menu menu--footer">
              <li class="menu__item">
                <a href="https://www.epa.gov/accessibility" class="menu__link">Accessibility</a>
              </li>
              <!--li class="menu__item"><a href="#" class="menu__link">EPA Administrator</a></li-->
              <li class="menu__item">
                <a href="https://www.epa.gov/planandbudget" class="menu__link">Budget &amp; Performance</a>
              </li>
              <li class="menu__item">
                <a href="https://www.epa.gov/contracts" class="menu__link">Contracting</a>
              </li>
              <li class="menu__item">
                <a href="https://www.epa.gov/home/wwwepagov-snapshots" class="menu__link">EPA www Web Snapshot</a>
              </li>
              <li class="menu__item">
                <a href="https://www.epa.gov/grants" class="menu__link">Grants</a>
              </li>
              <li class="menu__item">
                <a href="https://www.epa.gov/ocr/whistleblower-protections-epa-and-how-they-relate-non-disclosure-agreements-signed-epa-employees" class="menu__link">No FEAR Act Data</a>
              </li>
              <li class="menu__item">
                <a href="https://www.epa.gov/web-policies-and-procedures/plain-writing" class="menu__link">Plain Writing</a>
              </li>
              <li class="menu__item">
                <a href="https://www.epa.gov/privacy" class="menu__link">Privacy</a>
              </li>
              <li class="menu__item">
                <a href="https://www.epa.gov/privacy/privacy-and-security-notice" class="menu__link">Privacy and Security Notice</a>
              </li>
            </ul>
          </div>
          <div class="footer__column">
            <h2>Connect.</h2>
            <ul class="menu menu--footer">
              <li class="menu__item">
                <a href="https://www.data.gov/" class="menu__link">Data.gov</a>
              </li>
              <li class="menu__item">
                <a href="https://www.epa.gov/office-inspector-general/about-epas-office-inspector-general" class="menu__link">Inspector General</a>
              </li>
              <li class="menu__item">
                <a href="https://www.epa.gov/careers" class="menu__link">Jobs</a>
              </li>
              <li class="menu__item">
                <a href="https://www.epa.gov/newsroom" class="menu__link">Newsroom</a>
              </li>
              <li class="menu__item">
                <a href="https://www.epa.gov/data" class="menu__link">Open Government</a>
              </li>
              <li class="menu__item">
                <a href="https://www.regulations.gov/" class="menu__link">Regulations.gov</a>
              </li>
              <li class="menu__item">
                <a href="https://www.epa.gov/newsroom/email-subscriptions-epa-news-releases" class="menu__link">Subscribe</a>
              </li>
              <li class="menu__item">
                <a href="https://www.usa.gov/" class="menu__link">USA.gov</a>
              </li>
              <li class="menu__item">
                <a href="https://www.whitehouse.gov/" class="menu__link">White House</a>
              </li>
            </ul>
          </div>
          <div class="footer__column">
            <h2>Ask.</h2>
            <ul class="menu menu--footer">
              <li class="menu__item">
                <a href="https://www.epa.gov/home/forms/contact-epa" class="menu__link">Contact EPA</a>
              </li>
              <li class="menu__item">
                <a href="https://www.epa.gov/web-policies-and-procedures/epa-disclaimers" class="menu__link">EPA Disclaimers</a>
              </li>
              <li class="menu__item">
                <a href="https://www.epa.gov/aboutepa/epa-hotlines" class="menu__link">Hotlines</a>
              </li>
              <li class="menu__item">
                <a href="https://www.epa.gov/foia" class="menu__link">FOIA Requests</a>
              </li>
              <li class="menu__item">
                <a href="https://www.epa.gov/home/frequent-questions-specific-epa-programstopics" class="menu__link">Frequent Questions</a>
              </li>
            </ul>
            <h2>Follow.</h2>
            <ul class="menu menu--social">
              <li class="menu__item">
                <a class="menu__link" aria-label="EPA’s Facebook" href="https://www.facebook.com/EPA">
                  <!-- svg class="icon icon--social" aria-hidden="true" -->
                  <svg class="icon icon--social" aria-hidden="true" viewBox="0 0 448 512" id="facebook-square" xmlns="http://www.w3.org/2000/svg">
                    <!-- use xlink:href="https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#facebook-square"></use-->
                    <path fill="currentcolor" d="M400 32H48A48 48 0 000 80v352a48 48 0 0048 48h137.25V327.69h-63V256h63v-54.64c0-62.15 37-96.48 93.67-96.48 27.14 0 55.52 4.84 55.52 4.84v61h-31.27c-30.81 0-40.42 19.12-40.42 38.73V256h68.78l-11 71.69h-57.78V480H400a48 48 0 0048-48V80a48 48 0 00-48-48z"></path>
                  </svg> 
                  <span class="usa-tag external-link__tag" title="Exit EPA Website">
                    <span aria-hidden="true">Exit</span>
                    <span class="u-visually-hidden"> Exit EPA Website</span>
                  </span>
                </a>
              </li>
              <li class="menu__item">
                <a class="menu__link" aria-label="EPA’s Twitter" href="https://twitter.com/epa">
                  <!-- svg class="icon icon--social" aria-hidden="true" -->
                  <svg class="icon icon--social" aria-hidden="true" viewBox="0 0 448 512" id="twitter-square" xmlns="http://www.w3.org/2000/svg">
                    <!-- use xlink:href="https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#twitter-square"></use -->
                    <path fill="currentcolor" d="M400 32H48C21.5 32 0 53.5 0 80v352c0 26.5 21.5 48 48 48h352c26.5 0 48-21.5 48-48V80c0-26.5-21.5-48-48-48zm-48.9 158.8c.2 2.8.2 5.7.2 8.5 0 86.7-66 186.6-186.6 186.6-37.2 0-71.7-10.8-100.7-29.4 5.3.6 10.4.8 15.8.8 30.7 0 58.9-10.4 81.4-28-28.8-.6-53-19.5-61.3-45.5 10.1 1.5 19.2 1.5 29.6-1.2-30-6.1-52.5-32.5-52.5-64.4v-.8c8.7 4.9 18.9 7.9 29.6 8.3a65.447 65.447 0 01-29.2-54.6c0-12.2 3.2-23.4 8.9-33.1 32.3 39.8 80.8 65.8 135.2 68.6-9.3-44.5 24-80.6 64-80.6 18.9 0 35.9 7.9 47.9 20.7 14.8-2.8 29-8.3 41.6-15.8-4.9 15.2-15.2 28-28.8 36.1 13.2-1.4 26-5.1 37.8-10.2-8.9 13.1-20.1 24.7-32.9 34z"></path>
                  </svg>
                  <span class="usa-tag external-link__tag" title="Exit EPA Website">
                    <span aria-hidden="true">Exit</span>
                    <span class="u-visually-hidden"> Exit EPA Website</span>
                  </span>
                </a>
              </li>
              <li class="menu__item">
                <a class="menu__link" aria-label="EPA’s Youtube" href="https://www.youtube.com/user/USEPAgov">
                  <!-- svg class="icon icon--social" aria-hidden="true" -->
                  <svg class="icon icon--social" aria-hidden="true" viewBox="0 0 448 512" id="youtube-square" xmlns="http://www.w3.org/2000/svg">
                    <!-- use xlink:href="https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#youtube-square"></use -->
                    <path fill="currentcolor" d="M186.8 202.1l95.2 54.1-95.2 54.1V202.1zM448 80v352c0 26.5-21.5 48-48 48H48c-26.5 0-48-21.5-48-48V80c0-26.5 21.5-48 48-48h352c26.5 0 48 21.5 48 48zm-42 176.3s0-59.6-7.6-88.2c-4.2-15.8-16.5-28.2-32.2-32.4C337.9 128 224 128 224 128s-113.9 0-142.2 7.7c-15.7 4.2-28 16.6-32.2 32.4-7.6 28.5-7.6 88.2-7.6 88.2s0 59.6 7.6 88.2c4.2 15.8 16.5 27.7 32.2 31.9C110.1 384 224 384 224 384s113.9 0 142.2-7.7c15.7-4.2 28-16.1 32.2-31.9 7.6-28.5 7.6-88.1 7.6-88.1z"></path>
                  </svg>
                  <span class="usa-tag external-link__tag" title="Exit EPA Website">
                    <span aria-hidden="true">Exit</span>
                    <span class="u-visually-hidden"> Exit EPA Website</span>
                  </span>
                </a>
              </li>
              <li class="menu__item">
                <a class="menu__link" aria-label="EPA’s Flickr" href="https://www.flickr.com/photos/usepagov">
                  <!-- svg class="icon icon--social" aria-hidden="true" -->
                  <svg class="icon icon--social" aria-hidden="true" viewBox="0 0 448 512" id="flickr-square" xmlns="http://www.w3.org/2000/svg">
                    <!-- use xlink:href="https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#flickr-square"></use -->
                    <path fill="currentcolor" d="M400 32H48C21.5 32 0 53.5 0 80v352c0 26.5 21.5 48 48 48h352c26.5 0 48-21.5 48-48V80c0-26.5-21.5-48-48-48zM144.5 319c-35.1 0-63.5-28.4-63.5-63.5s28.4-63.5 63.5-63.5 63.5 28.4 63.5 63.5-28.4 63.5-63.5 63.5zm159 0c-35.1 0-63.5-28.4-63.5-63.5s28.4-63.5 63.5-63.5 63.5 28.4 63.5 63.5-28.4 63.5-63.5 63.5z"></path>
                  </svg>
                  <span class="usa-tag external-link__tag" title="Exit EPA Website">
                    <span aria-hidden="true">Exit</span>
                    <span class="u-visually-hidden"> Exit EPA Website</span>
                  </span>
                </a>
              </li>
              <li class="menu__item">
                <a class="menu__link" aria-label="EPA’s Instagram" href="https://www.instagram.com/epagov">
                  <!-- svg class="icon icon--social" aria-hidden="true" -->
                  <svg class="icon icon--social" aria-hidden="true" viewBox="0 0 448 512" id="instagram-square" xmlns="http://www.w3.org/2000/svg">
                    <!-- use xlink:href="https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#instagram-square"></use -->
                    <path fill="currentcolor" xmlns="http://www.w3.org/2000/svg" d="M224 202.66A53.34 53.34 0 10277.36 256 53.38 53.38 0 00224 202.66zm124.71-41a54 54 0 00-30.41-30.41c-21-8.29-71-6.43-94.3-6.43s-73.25-1.93-94.31 6.43a54 54 0 00-30.41 30.41c-8.28 21-6.43 71.05-6.43 94.33s-1.85 73.27 6.47 94.34a54 54 0 0030.41 30.41c21 8.29 71 6.43 94.31 6.43s73.24 1.93 94.3-6.43a54 54 0 0030.41-30.41c8.35-21 6.43-71.05 6.43-94.33s1.92-73.26-6.43-94.33zM224 338a82 82 0 1182-82 81.9 81.9 0 01-82 82zm85.38-148.3a19.14 19.14 0 1119.13-19.14 19.1 19.1 0 01-19.09 19.18zM400 32H48A48 48 0 000 80v352a48 48 0 0048 48h352a48 48 0 0048-48V80a48 48 0 00-48-48zm-17.12 290c-1.29 25.63-7.14 48.34-25.85 67s-41.4 24.63-67 25.85c-26.41 1.49-105.59 1.49-132 0-25.63-1.29-48.26-7.15-67-25.85s-24.63-41.42-25.85-67c-1.49-26.42-1.49-105.61 0-132 1.29-25.63 7.07-48.34 25.85-67s41.47-24.56 67-25.78c26.41-1.49 105.59-1.49 132 0 25.63 1.29 48.33 7.15 67 25.85s24.63 41.42 25.85 67.05c1.49 26.32 1.49 105.44 0 131.88z"></path>
                  </svg>
                  <span class="usa-tag external-link__tag" title="Exit EPA Website">
                    <span aria-hidden="true">Exit</span>
                    <span class="u-visually-hidden"> Exit EPA Website</span>
                  </span>
                </a>
              </li>
            </ul>
            <p class="footer__last-updated">
              Last updated on March 30, 2022
            </p>
          </div>
        </div>
      </div>
    </footer>
    <a href="#" class="back-to-top" title="">
      <svg class="back-to-top__icon" role="img" aria-label="">
      <svg class="back-to-top__icon" role="img" aria-label="" viewBox="0 0 19 12" id="arrow" xmlns="http://www.w3.org/2000/svg">
        <!-- use xlink:href="https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#arrow"></use -->
        <path fill="currentColor" d="M2.3 12l7.5-7.5 7.5 7.5 2.3-2.3L9.9 0 .2 9.7 2.5 12z"></path>
      </svg>
    </a>'
    )
    
    
    
    
  )  # Close fluid page
  
  
  
  # Define server logic to read and analyze files to produce figures and reports ----
  server <- function(input, output, session) {

    starttime<-Sys.time()

    output$clock <- shiny::renderText({
      shiny::invalidateLater(5000)
      myclock<-Sys.time()-starttime
      ##paste(as.numeric(round(myclock,1)),units(myclock))
      num<-as.numeric(round(myclock,2))*100
      if(num %% 2 == 0){return("..")}
      if(num %% 2 != 0){return("....")}
    })
        # App Settings -----
    # # Increase the maximum size allowable for file uploads -----
    options(shiny.maxRequestSize = 100 * 1024 ^ 2)

    # Tab 1: Instructions & Documents -----
    # # DT output: docs_tbl -----
    # # This editable table will display the name(s) of all uploaded document(s)
    # # # UI output: ui_doc_message -----
    output$ui_doc_message <- shiny::renderUI({

      # If there is no data in docs_dt() (no uploaded document[s]), display a message indicating document(s) have not yet been uploaded
      if(NROW(docs_dt()) == 0){
        # Disable options if there are no documents
        shinyjs::disable("find_matches_btn")

        return(htmltools::div("No documents uploaded", style = "font-size: 12pt;"))
      }
      
      
      # If there is data in docs_dt() (document[s] uploaded), create a data table output of the document(s)
      DT::DTOutput("docs_tbl")
      
    })  # Close renderUI
    
    # # # reactiveVal: docs_dt() -----
    # # Create an empty reactive data frame to house uploaded document(s) information
    docs_dt <- shiny::reactiveVal(dplyr::tibble())
    
    
    # # # observeEvent: docs_upload -----
    # # # When a document is uploaded, add information about it to the reactive data frame docs_dt() and enable/disable features
    shiny::observeEvent(input$docs_upload, {

      ###Provide a warning that documents have been uploaded
      shinyWidgets::show_alert("Document Upload Complete!", "Use the 'Find Matches' button to run keyword search on documents.", type = "success")
  
            
      # Create a temporary data frame of uploaded document(s) information
      temp_docs <- rbind(dplyr::as_tibble(input$docs_upload), docs_dt()) %>%
        dplyr::distinct(name, .keep_all = TRUE)
      
      if(NROW(docs_dt()) == 0) {
        removed <- paste("The following documents were unable to be uploaded because they are corrupted, damaged, or otherwise unreadable:",
                         stringr::str_c(dplyr::filter(temp_docs, size == 0)$name, collapse = ",\n"))
        if(NROW(dplyr::filter(temp_docs, size == 0)) > 0) {
          shinyWidgets::show_alert(title = "Upload error!",
                                   text = removed,
                                   type = "warning")
        }
      } else {
        new_docs <- dplyr::anti_join(temp_docs, docs_dt(), by = "name")
        removed <- paste("The following documents were unable to be uploaded because they are corrupted, damaged, or otherwise unreadable:",
                         stringr::str_c(dplyr::filter(new_docs, size == 0)$name, collapse = ",\n"))
        if(NROW(dplyr::filter(new_docs, size == 0)) > 0) {
          shinyWidgets::show_alert(title = "Upload error!",
                                   text = removed,
                                   type = "warning")
        }
      }
      
      # Add document(s) information to the existing table of document(s) information
      docs_dt(temp_docs)
      
      # Enable "Remove selected file(s)" and "Find Matches" buttons and the number of lines drop-down menu
      shinyjs::enable("remove_docs_btn")
      shinyjs::enable("find_matches_btn")
      
    })  # Close observeEvent
    
    
    # # # renderDT: docs_tbl -----
    # # # Render a data table of the uploaded document(s) information contained in docs_dt()
    output$docs_tbl <- DT::renderDT({
      
      docs_dt() |>
        # Remove unreadable documents
        dplyr::filter(size > 0) |>
        # Keep only the document name(s)
        dplyr::select(name) |>
        # Do not include column names, show only the table and page control, and establish table styling
        DT::datatable(colnames = "",
                      options = list(dom = "pt"),
                      class = "row-border")
      
    })  # Close renderDT
    
    
    # # # observeEvent: remove_docs_btn -----
    shiny::observeEvent(input$remove_docs_btn, {
      
      # Create a temporary object to represent the reactive data frame docs_dt()
      r <- docs_dt()
      
      # If a row is selected in the data table (not NULL), remove the row from the temporary data table by row number(s)
      if(!is.null(input$docs_tbl_rows_selected)) {
        r <- r[-as.numeric(input$docs_tbl_rows_selected),]
      }
      
      # Update the reactive data frame docs_dt() to exclude the removed row(s)
      docs_dt(r)
      
    })  # Close observeEvent
    
    
    # # # renderDT: docs_tbl (document[s] removed) -----
    # # # Re-render the data table of documents with the updated docs_dt() data frame
    output$docs_tbl <- DT::renderDT({
      
      docs_dt() |>
        # Remove unreadable documents
        dplyr::filter(size > 0) |>
        # Keep only the document name(s)
        dplyr::select(name) |>
        # Do not include column names, show only the table and page control, and establish table styling
        DT::datatable(colnames = "",
                      options = list(dom = "pt"),
                      class = "row-border")
      
    })  # Close renderDT
    
    
    # # DT output: matches_tbl -----
    # # This table will display all cleaned triplet matches found in the uploaded document(s)
    # # # UI output: ui_matches_message -----
    output$ui_matches_message <- shiny::renderUI({
      
      # If there is no data in trip_clean(), display a message that matches have not yet been found
      if(NROW(trip_clean()) == 0)
        return(htmltools::div("Upload documents then click 'Find Matches' to search for keyword matches. Any single document must be less than 100MB, but one or more (up to dozens) of documents can be uploaded and analyzed. Finding Matches can take 10-30 seconds per document, or >60 seconds for large document sizes. Closing or minimizing the browser window, or lack of activity that causes computer to go to sleep or standby, may cause the server to timeout and results will not be saved automatically. Export results as soon as possible after finding matches to ensure results are saved. Output is based on an automated search for keywords in documents that has been reviewed, refined, and tested to optimize accuracy in the way sentences are categorized, but due to the complex and variable nature of written language, false hits cannot be eliminated entirely. Users are responsible for evaluating applicability, precision, accuracy, uncertainty, and other qualifications associated with usability of results.", style = "font-size: 12pt;"))
      
      # If there is data in trip_clean(), create a UI output indicating the status of the matches
      shiny::uiOutput("ui_match_tbl_message")
      
    })  # Close renderUI
    
    
    # # # reactiveVal: trip_clean() -----
    # # # Create an empty reactive data frame to house cleaned triplet matches
    trip_clean <- shiny::reactiveVal(dplyr::tibble())
    
    
    # # # reactiveVal: efreq() -----
    # # Create an empty reactive data frame to house ecosystem frequencies in documents
    efreq <- shiny::reactiveVal(dplyr::tibble())
    
    
    # # # reactiveVal: mean_efreq() -----
    # # Create an empty reactive data frame to house mean ecosystem frequencies
    mean_efreq <- shiny::reactiveVal(dplyr::tibble())
    
    
    # # # reactiveVal: eco_bar_fig() -----
    # # Create an empty reactive list to house the ecosystem frequency figure
    eco_bar_fig <- reactiveVal()
    
    
    # # # reactiveVal: sefreq() -----
    # # Create an empty reactive data frame to house specific ecosystem frequencies in documents
    # sefreq <- shiny::reactiveVal(dplyr::tibble())
    
    
    # # # reactiveVal: eco_pie_list() -----
    # # Create an empty reactive list to house the specific ecosystem frequencies pie charts
    eco_pie_fig <- reactiveVal()
    
    
    # # # reactiveVal: bfreq() -----
    # # Create an empty reactive data frame to house beneficiary frequencies in documents
    bfreq <- shiny::reactiveVal(dplyr::tibble())
    
    
    # # # reactiveVal: mean_bfreq() -----
    # # Create an empty reactive data frame to house mean beneficiary frequencies
    mean_bfreq <- shiny::reactiveVal(dplyr::tibble())
    
    
    # # # reactiveVal: ben_bar_list() -----
    # # Create an empty reactive list to house beneficiary frequencies bar graphs
    ben_bar_list <- shiny::reactiveVal(list())
    
    
    # # # reactiveVal: ben_pie_list() -----
    # # Create an empty reactive list to house beneficiary frequencies pie graphs
    ben_pie_list <- shiny::reactiveVal(list())
    
    
    # # # reactiveVal: ffreq() -----
    # # Create an empty reactive data frame to house FEGS frequencies in documents
    ffreq <- shiny::reactiveVal(dplyr::tibble())
    
    
    # # # reactiveVal: mean_ffreq() -----
    # # Create an empty reactive data frame to house mean FEGS frequencies
    mean_ffreq <- shiny::reactiveVal(dplyr::tibble())
    
    
    # # # reactiveVal: fegs_bar_list() -----
    # # Create an empty reactive list to house services frequencies bar graphs
    fegs_bar_list <- shiny::reactiveVal(list())
    
    
    # # # reactiveVal: fegs_pie_list() -----
    # # Create an empty reactive list to house services frequencies bar graphs
    fegs_pie_list <- shiny::reactiveVal(list())
    
    
    # # # reactiveVal: mean_fscore -----
    # Create an empty reactive data frame to house mean FEGS scores
    mean_fscore <- shiny::reactiveVal(dplyr::tibble())

    

        
    # # # observeEvent: find_matches_btn -----
    shiny::observeEvent(input$find_matches_btn, {

      
      # Disable all enabled options
      shinyjs::disable("docs_upload")
      shinyjs::disable("find_matches_btn")
      shinyjs::disable("remove_docs_btn")
      
      # Enable "Cancel" button
      shinyjs::enable("cancel_btn")
      
      ###Provide a warning that the search is running
      shinyWidgets::show_alert("Note!", "Document search is running, and can take 10-30 seconds per document, or >60 seconds for large documents. A progress bar is at the top of the window.", type = "warning")
      
      # Temporarily save docs_dt()
      documents_df <- dplyr::filter(docs_dt(), size > 0)
      usethis::use_data(documents_df, overwrite = TRUE)
      
      
      shiny::withProgress(message = "Searching documents...", value = 0, style = "old", {

        # Increase progress bar
        shiny::incProgress(amount = 1/2)

        # Find keyword matches
        targets::tar_make()
        keyword_matches <- targets::tar_read(keyword_matches)
      
        if(NROW(keyword_matches) == 0) {
          ###Close the warning alert about how long it takes to run
          shinyalert::closeAlert(num = 0, id = NULL)
          
          shinyWidgets::show_alert(title = "No keywords matches have been found in the uploaded document(s)!",
                                 text = "There may be no matching keywords, or this may be due to an error if the pdf document does not contain searchable text.",
                                 type = "warning")
        } else {
          keyword_matches <- dplyr::filter(keyword_matches, !is.na(`Line Number`))
        }
      
        # Increase progress bar
        shiny::incProgress(amount = 2/2)
        
        
              }) ##Close Progress    
      
      
    if(NROW(keyword_matches) > 0) {
        # Perform triplet search, counts, and cleaning with a progress bar
        shiny::withProgress(message = "Filtering keyword matches...", value = 0, style = "old", {
        
           # Increase progress bar
           shiny::incProgress(amount = 1/3)
        
           # Find all triplets from keyword matches
           trips_all <- find_triplets(keyword_matches, 1, synonym_list_all)
        
           if(NROW(trips_all) > 0) {
        
           # Increase progress bar
           shiny::incProgress(amount = 2/3)
        
           # Count triplets from documents
           trip_count <- count_triplets(trips_all)
        
           # Increase progress bar
           shiny::incProgress(amount = 3/3)
        
           # Clean triplets
           clean <- clean_triplets(trips_all, trip_count, 1)
        
           # Add cleaned triplets to the reactive empty data frame trip_clean()
           trip_clean(clean)
        
          }
        
         })  # Close withProgress
      
         if(NROW(trip_clean()) > 0) {
        
           # Calculate frequencies with a progress bar
           shiny::withProgress(message = "Generating profile figures...", value = 0, style = "old", {
          
             # Increase progress bar
             shiny::incProgress(amount = 1/4)
          
             # Calculate ecosystem frequencies in documents
             efreq(eco_freq(trip_clean()))
          
             # Calculate mean ecosystem frequencies
             mean_efreq(mean_eco_freq(efreq()))
          
             # Create ecosystem frequency figure
             eco_bar_fig(eco_bars(mean_efreq()))
             
             # Calculate specific ecosystem frequencies in documents
             # sefreq(sp_eco_freq(trip_clean()))
             
             # Create specific ecosystem frequencies pie charts
             eco_pie_fig(eco_pies(mean_efreq()))
          
             # Increase progress bar
             shiny::incProgress(amount = 2/4)
           
             # Calculate beneficiary frequencies in documents
             bfreq(ben_freq(trip_clean()))
          
             # Calculate mean beneficiary frequencies
             mean_bfreq(mean_ben_freq(bfreq()))
          
             # Create beneficiary bar figures
             ben_bar_list(eco_ben_bars(mean_bfreq()))
          
             # Create beneficiary pie figures
             ben_pie_list(eco_ben_pies(mean_bfreq()))
          
             # Increase progress bar
             shiny::incProgress(amount = 3/4)
          
             # Calculate FEGS frequencies in documents
             ffreq(fegs_freq(trip_clean()))
          
             # Calculate mean FEGS frequencies
             mean_ffreq(fegs_wgt_mean(ffreq(), bfreq()))
          
             # Calculate mean FEGS scores
             mean_fscore(fegs_score(ffreq(), bfreq()))
          
             # Create services bar figures
             fegs_bar_list(eco_fegs_bars(mean_ffreq()))
          
             # Create services pie figures
             fegs_pie_list(eco_fegs_pies(mean_fscore()))
          
             # Increase progress bar
             shiny::incProgress(amount = 4/4)
          

           })  # Close withProgress
        
           ###Close the warning alert about how long it takes to run
           shinyalert::closeAlert(num = 0, id = NULL)
       
           
           # Show an alert when the process has completed
           shinyWidgets::show_alert(title = "Search complete!",
                                 text = "Figures may be viewed on profile panels",
                                 type = "success")
        
         } else {
           ###Close the warning alert about how long it takes to run
           shinyalert::closeAlert(num = 0, id = NULL)
           
           shinyWidgets::show_alert(title = "No keyword matches have been found in the uploaded document(s)!",
                                 type = "warning")
         }
    }    
      # # # # UI output: ui_match_tbl_message -----
      output$ui_match_tbl_message <- shiny::renderUI({
        
        # If there is no data in trip_clean(), display an alert indicating no matches were found
        if(NROW(trip_clean()) > 0) {
          # If there is data in trip_clean(), create a DT output of triplet matches
          ##DT::DTOutput("matches_tbl")
          div(DT::DTOutput("matches_tbl"), style = "font-size:85%")
       }
        

              })  # Close renderUI
      
      # # # # renderText: eco_title -----
      # Create title for ecosystem figure
      output$eco_title <- shiny::renderText({
        
          "Ecosystems Mentioned in Document(s)"

      })  # Close renderText
      
      

      # # # # renderText: ben_title -----
      # Create title for beneficiary figures
      output$ben_title <- shiny::renderText({
        
        paste("Beneficiary Prioritization for", input$eco_dd)
        
      })  # Close renderText
      
      # # # # renderText: fegs_title -----
      # Create title for ecosystem figure
      output$fegs_title <- shiny::renderText({
        
        paste("Environmental Attributes Prioritization for", input$eco_dd)
        
      })  # Close renderText

      
      # # # # renderText: wordcloud_title -----
      # Create title for ecosystem figure
      output$wordcloud_title <- shiny::renderText({
        
        paste("Word Cloud of Document Text for", input$eco_dd)

      })  # Close renderText
      

      
      # Create description for ecosystem figure
      output$eco_description <- shiny::renderText({
        if(input$plot_type == "bar") {
          return("Frequency of ecosystem level II subclasses in documents, based on the relative frequency mentioned in documents. Labels help visualize NESCS Plus hierarchy, with classes in bold, subclass I in italics, and subclass II in plain font. Ecosystems labelled ‘in general’ are NOT summary or cumulative of their nested subclasses, but instead count sentences that could not be further classified to a finer subclass.")
        } else {
          return("Frequency of ecosystem level I subclasses in documents, based on the relative frequency mentioned in documents. Aquatic or Terrestrial Ecosystem 'in General' are sentences that could not be classified into a finer subclass.")
        }
      })  # Close renderText
      
      # Create description for beneficiary figure
      output$ben_description <- shiny::renderText({
        if(input$plot_type == "bar") {
          return("Relative prioritization scores of beneficiary level I subclasses in documents, based on the relative frequency mentioned in documents. Labels help visualize NESCS Plus hierarchy, with classes in bold and subclass I in plain font. Beneficiaries labelled ‘in general’ are NOT summary or cumulative of their nested subclasses, but instead count sentences that could not be further classified to a finer subclass.")
        } else {
          return("Relative prioritization scores of beneficiary classes in documents, based on the relative frequency mentioned in documents.")
        }
      })  # Close renderText
      
      # Create description for attribute figure
      output$fegs_description <- shiny::renderText({
        if(input$plot_type == "bar") {
          return("Relative prioritization scores of environmental attribute subclasses in documents, based on summing their relative importance to each beneficiary. Stacked colors indicate the relative contribution of each beneficiary class to the overall prioritization score. Labels help visualize NESCS Plus hierarchy, with classes in bold, subclasses in plain font, and split classes (where applicable) in italics. Environmental attributes labelled ‘in general’ are NOT summary or cumulative of their nested subclasses, but instead count sentences that could not be further classified to a finer subclass.")
        } else {
          return("Relative prioritization scores of environmental attribute classes in documents, based on summing their relative importance to each beneficiary. 'Composite' is further portioned into split classes, with 'composite in general' applied to sentences that could not be categorized into a finer 'composite' subclass.")
        }
      })  # Close renderText

      # Create description for word cloud figure
      output$wordcloud_description <- shiny::renderText({
          "Font size shows relative frequency of actual words in documents, from sentences used to generate environment, beneficiary, and attribute profiles. Mouse hover over each word to see the number of unique sentences that contained that word."
      })  # Close renderText
      
      # Re-enable disabled options and enable the ecosystem drop-down menu, plot type radio buttons, and download options
      shinyjs::enable("docs_upload")
      shinyjs::enable("find_matches_btn")
      shinyjs::enable("create_figs_btn")
      shinyjs::enable("remove_docs_btn")
      shinyjs::enable("download_report_btn")
      shinyjs::enable("eco_dd")
      shinyjs::enable("plot_type")
      shinyjs::enable("export_res_btn")
      shinyjs::enable("file_type")
      shinyjs::enable("eco_fig_export")
      shinyjs::enable("eco_fig_sel")
      shinyjs::enable("export_fig_btn")
      
      # Disable "Cancel" button
      shinyjs::disable("cancel_btn")
      
      if(NROW(trip_clean() > 0)) {
        # Update ecosystems available for export
        shiny::updateSelectInput("eco_dd",
                                 session = session,
                                 choices = sort(unique(dplyr::filter(mean_ffreq(), Mean_FEGS_Score > 0)$DR_Eco_SubClass)))
        # Update ecosystems available for export
        shiny::updateCheckboxGroupInput("eco_fig_sel",
                                        session = session,
                                        choices = sort(unique(dplyr::filter(mean_ffreq(), Mean_FEGS_Score > 0, DR_Eco_SubClass != "All Ecosystems")$DR_Eco_SubClass)))
      }

      
    })  # Close observeEvent
    

    # # # renderDT: matches_tbl -----
    # # # Render results of "Find Matches" as a data table of cleaned triplet matches
    output$matches_tbl <- DT::renderDT({
      
      DT::datatable(
        # Keep only the document name, Environment SubClass, beneficiary subclass, Attribute SubClass, and the matched sentence text
        dplyr::select(trip_clean(), Document, DR_Eco_SubClass, DR_Ben_SubClass, DR_FEGS_SubClass, Paragraph_Text),
        # Edit the column names
        colnames = c("Document", "Environment SubClass", "Beneficiary SubClass", "Attribute SubClass", "Matched Document Text"),
        # Set table options
        options = list(
          # Set column options
          columnDefs = list(
            # Format columns 5
            list(targets = c(5),
                 # Cut text longer than 200 characters and replace with ...
                 render = DT::JS(
                   "function(data, type, row, meta) {",
                   "return type === 'display' && data.length > 200 ?",
                   "'<span title=\"' + data + '\">' + data.substr(0, 200) + '...</span>' : data;",
                   "}"
                   )),
            # Format columns 1
            list(targets = c(1),
                 # Cut text longer than 20 characters and replace with ...
                 render = DT::JS(
                   "function(data, type, row, meta) {",
                   "return type === 'display' && data.length > 20 ?",
                   "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                   "}"
                 )),
            # Format columns 2, 3, & 4
            list(targets = c(2, 3, 4),
                 # Set the width to 130 pixels
                 width = "130px")
          )  # Close column formatting
        )  # Close table options
      )     # Close datatable

    })  # Close renderDT
    
    
    # Tab 2: Ecosystem profile -----
    # # UI output: ui_eco_bars_message -----
    output$ui_eco_bars_message <- shiny::renderUI({
      
      # If there is no data in mean_efreq(), display a message that ecosystem figures haven't yet been created
      if(NROW(mean_efreq()) == 0)
        return(shiny::strong("An ecosystem profile figure has not yet been created. Please upload document(s) and follow the instructions found in the 'Instructions & Documents' panel.",
                             style = "font-size: 14pt;"))

      # If there is data in mean_efreq(), create a plot output for the ecosystem frequency figure
      # Active a "spinner" when figures are loading
      shinycssloaders::withSpinner(

        shiny::plotOutput("eco_figs", width = "90%", height = ifelse(input$plot_type == "bar", 600, 850)),
        type = 5, proxy.height="200px")
      
    })  # Close renderUI
    
    
    # # Plot output: eco_figs -----
    output$eco_figs <- shiny::renderPlot({
      
      if(input$plot_type == "bar") {
        # Render the ecosystem frequencies in document(s) figure
        max_val <- plyr::round_any(max(mean_efreq()$Mean_Ecos_Score, na.rm = TRUE), 10, ceiling) + 2.5
        eco_bar_fig() +
          ggplot2::scale_y_continuous(limits = c(0, max_val), expand = c(0, 0))
      } else {
        # If the radio button selection is "pie", create a pie chart
        eco_pie_fig()
      }
      
    }, res = 100) # Close renderPlot

        

    
    
    # Tab 3: Ecosystem beneficiary profile -----
    # # UI output: ui_ben_figs_message -----
    output$ui_ben_figs_message <- shiny::renderUI({
      
      # If there is no data in mean_bfreq(), display a message that beneficiary figures haven't yet been created
      if(NROW(mean_bfreq()) == 0)
        return(shiny::strong("Beneficiary profile figures have not yet been created. Please upload document(s) and follow the instructions found in the 'Instructions & Documents' panel.",
                             style = "font-size: 14pt;"))
      
      # If there is data in mean_efreq(), create a UI output indicating the status of the beneficiary frequency figures
      shiny::uiOutput("ui_ben_figs")
      
    })  # Close renderUI
    
    
    # # UI output: ui_ben_figs -----
    output$ui_ben_figs <- shiny::renderUI({
      
      # If there are no beneficiaries for the ecosystem selected from the drop-down menu, display an alert indicating so
      if(dplyr::near(max(dplyr::filter(mean_bfreq(), DR_Eco_SubClass == input$eco_dd)$Mean_Ben_Score, na.rm = TRUE), 0) == TRUE)
        return(shinyWidgets::show_alert(title = "Not found in the uploaded document(s)!",
                                        text = "Please choose a different ecosystem",
                                        type = "warning"))
      
      tagList(
        # If there is data in mean_efreq() and the ecosystem is present, create a plot output for the beneficiary frequency figures
        # Add the legend as a separate element to control width with screen size
        htmltools::div(class = "ben_leg",
                       shiny::plotOutput("ben_leg",
                                         height = "100px")),
        # Active a "spinner" when figures are loading
        shinycssloaders::withSpinner(
          shiny::plotOutput("ben_fig",
                            # Set the plot height based on the plot type selected by the radio buttons
                            height = dplyr::filter(dims, profile == "ben", plot_type == input$plot_type)$height,
                            # Set the plot width based on the plot type selected by the radio buttons
                            width = dplyr::filter(dims, profile == "ben", plot_type == input$plot_type)$width),
          type = 5, proxy.height="200px")
      )
    })  # Close renderUI
    
    
    # # Plot output: ben_leg -----
    output$ben_leg <- shiny::renderPlot({
      if(input$plot_type == "bar") {
        # Extract the beneficiary bar figure legend
        ben_leg <- ggpubr::get_legend(ben_bar_list())
        # Convert legend to ggplot
        ggpubr::as_ggplot(ben_leg)
      }
    })
    
    
    # # Plot output: ben_fig -----
    output$ben_fig <- shiny::renderPlot({
      
      if(input$plot_type == "bar") {
        # If the radio button selection is "bar", create a bar chart
        max_val <- plyr::round_any(max(dplyr::filter(mean_bfreq(), DR_Eco_SubClass == input$eco_dd)$Mean_Ben_Score, na.rm = TRUE), 10, ceiling) + 2.5
        ben_bar_list()[[input$eco_dd]] +
          ggplot2::theme(legend.position = "none") +
          ggplot2::scale_y_continuous(limits = c(0, max_val), expand = c(0, 0))
      } else {
        # If the radio button selection is "pie", create a pie chart
        ben_pie_list()[[input$eco_dd]]
      }
      
    }, res = 100) # Close renderPlot
    
    
    # Tab 4: Ecosystem services profile -----
    # # UI output: ui_fegs_figs_message -----
    output$ui_fegs_figs_message <- shiny::renderUI({
      
      # If there is no data in mean_ffreq(), display a message that ecosystem services figures haven't yet been created
      if(NROW(mean_ffreq()) == 0)
        return(shiny::strong("Ecosystem services profile figures have not yet been created. Please upload document(s) and follow the instructions found in the 'Instructions & Documents' panel.",
                             style = "font-size: 14pt;"))
      
      # If there is data in mean_ffreq(), create a UI output indicating the status of the ecosystem services frequency figures
      shiny::uiOutput("ui_fegs_figs")
      
    })  # Close renderUI
    

    # Tab 5: Word Cloud -----
    # # UI output: ui_wordcloud_message -----
    output$ui_wordcloud_figs_message <- shiny::renderUI({
      
      # If there is no data in mean_ffreq(), display a message that ecosystem services figures haven't yet been created
      if(NROW(mean_ffreq()) == 0)
        return(shiny::strong("Word cloud of document text has not yet been created. Please upload document(s) and follow the instructions found in the 'Instructions & Documents' panel.",
                             style = "font-size: 14pt;"))
      
      shiny::strong("wordcloud_title",
                    style = "font-size: 16pt;")
      
      # If there is data in mean_ffreq(), create a UI output indicating wordcloud
      wordcloud2::wordcloud2Output("ui_wordcloud", 
                                   width = "100%", height = 900)

    })  # Close renderUI
    
        
    # # UI output: ui_fegs_figs -----
    output$ui_fegs_figs <- shiny::renderUI({
      
      # If there are no ecosystem services for the ecosystem selected from the drop-down menu, display an alert indicating so
      if(dplyr::near(max(dplyr::filter(mean_ffreq(), DR_Eco_SubClass == input$eco_dd)$Mean_FEGS_Score, na.rm = TRUE), 0) == TRUE)
        return(shinyWidgets::show_alert(title = "Not found in the uploaded document(s)!",
                                        text = "Please choose a different ecosystem",
                                        type = "warning"))
      
      htmltools::tagList(
        # If there is data in mean_ffreq() and the ecosystem is present, create a plot output for the ecosystem services frequency figures
        # Add the legend as a separate element to control width with screen size
        htmltools::div(class = "fegs_leg",
                       shiny::plotOutput("fegs_leg",
                                         height = "100px")),
        # Active a "spinner" when figures are loading
        shinycssloaders::withSpinner(
          shiny::plotOutput("fegs_fig",
                            # Set the plot height based on the plot type selected by the radio buttons
                            height = dplyr::filter(dims, profile == "fegs", plot_type == input$plot_type)$height,
                            # Set the plot width based on the plot type selected by the radio buttons
                            width = dplyr::filter(dims, profile == "fegs", plot_type == input$plot_type)$width),
          type = 5, proxy.height="200px")
      )
    })  # Close renderUI
    
    
    # # Plot output: fegs_leg -----
    output$fegs_leg <- shiny::renderPlot({
      if(input$plot_type == "bar") {
        # Extract the beneficiary bar figure legend
        fegs_leg <- ggpubr::get_legend(fegs_bar_list())
        # Convert legend to ggplot
        ggpubr::as_ggplot(fegs_leg)
      }
    })
    
    
    # # Plot output: fegs_fig -----
    output$fegs_fig <- shiny::renderPlot({
      
      if(input$plot_type == "bar") {
        # If the radio button selection is "bar", create a bar chart
        max_val <- plyr::round_any(max(dplyr::summarize(dplyr::group_by(dplyr::filter(mean_ffreq(), DR_Eco_SubClass == input$eco_dd),
                                                                        DR_Eco_SubClass, DR_FEGS_SubClass),
                                                        Total = sum(Mean_FEGS_Score))$Total, na.rm = TRUE), 10, ceiling) + 2.5
        fegs_bar_list()[[input$eco_dd]] +
          ggplot2::theme(legend.position = "none") +
          ggplot2::scale_y_continuous(limits = c(0, max_val), expand = c(0, 0))
      } else {
        # If the radio button selection is "pie", create a pie chart
        fegs_pie_list()[[input$eco_dd]]
      }
      
    }, res = 100) # Close renderPlot
    
    
    # # UI output: ui_wordcloud -----
    output$ui_wordcloud <- wordcloud2::renderWordcloud2(my_wordcloud(trip_clean(),input$eco_dd))

    
        
    # Action button: cancel_btn -----
    # NEED TO FIGURE THIS OUT
    
    
    # Download: export_res_btn -----
    # # reactiveVal: res_wb -----
    # # Create an empty reactive value to contain the Excel workbook formatted data
    res_wb <- shiny::reactiveVal()
    
    # # reactiveVal: res_fegs() -----
    # # Create an empty reactive value to contain the FST formatted data
    res_fegs <- shiny::reactiveVal()
    
    
    # # downloadHandler: export_res_btn -----
    output$export_res_btn <- shiny::downloadHandler(
      
      # Set the file name of the export
      filename = function() {
        if(input$file_type == "xl") {
          paste0("FEGS_DocReader_Results_", Sys.Date(), ".xlsx")
        } else {
          paste0("FEGS_DocReader_FST_", Sys.Date(), ".zip")
        }
        
      },
      
      # Set the content of the export
      content = function(file) {
        
        # Export figures with a progress bar
        shiny::withProgress(message = "Preparing results for export...", value = 0, style = "old", {
          
          if(input$file_type == "xl") {
            # Set up the export workbook
            res_wb(export_wb(trip_clean(), mean_efreq(), mean_bfreq(), mean_ffreq(), mean_fscore()))
            # Save the workbook
            openxlsx::saveWorkbook(res_wb(), file)
            
            # Show an alert when the process has completed
            shinyWidgets::show_alert(title = "File saved!",
                                     type = "success")
          } else {
            # Calculate the relative importance of non-tier 1 beneficiaries
            corr_ben_relimp <- ben_relimp(bfreq(), Friendly_Names_upd)
            # Redistribute tier 1 beneficiary scores
            ben_redist <- ben_score_redist(bfreq(), corr_ben_relimp, Friendly_Names_upd)
            # Redistribute tier 1 beneficiaries' attribute scores based on relative importance
            fegs_redist <- attr_redist_by_ben(corr_ben_relimp, mean_ffreq(), Friendly_Names_upd)
            # Redistribute site appeal attribute scores
            site_redist <- site_appeal_redist(fegs_redist, Friendly_Names_upd)
            # Redistribute tier 1 attribute scores based on relative importance
            attr_redist <- attr_score_redist(fegs_redist, site_redist, Friendly_Names_upd)
            # Convert beneficiary scores to FST format
            fst_ben <- ben_fst_format(ben_redist)
            # Convert attribute scores to FST format
            fst_attr <- fegs_fst_format(attr_redist)
            # Merge beneficiary scores, attribute scores, and the FST header
            res_fegs(fst_by_eco(fst_ben, fst_attr, fst_header()))
            
            # Save .fegs files by ecosystem in a single .zip file
            # Set the working directory as a temporary location before zipping
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            
            # Create a vector of names for .fegs files
            file_names <- paste0("FEGS_DocReader_FST_", stringr::str_replace_all(names(res_fegs()), " ", "_"), "_", Sys.Date(), ".fegs")
            
            # Save FST formatted data
            for(n in names(res_fegs())) {
              write.table(noquote(paste0('{', res_fegs()[[n]], '}')),
                          file = paste0("FEGS_DocReader_FST_", stringr::str_replace_all(n, " ", "_"), "_", Sys.Date(), ".fegs"),
                          row.names = FALSE, col.names = FALSE, quote = FALSE)
            }
            
            # Add the .fegs files to the .zip file and save
            zip(file, files = file_names)
            
            # Show an alert when the process has completed
            shinyWidgets::show_alert(title = "File saved!",
                                     type = "success")
          }
          
        })  # Close withProgress
        
      }
      
    ) # Close downloadHandler
    
    
    # Download: export_fig_btn -----
    # # reactiveVal: has_data -----
    # # Create an empty reactive data frame to house ecosystems with frequency data
    has_data <- shiny::reactiveVal()
    
    
    # # downloadHandler: export_fig_btn -----
    output$export_fig_btn <- shiny::downloadHandler(
      
      # Set the file name of the export
      filename = paste0("FEGS_DocReader_Figures_", Sys.Date(), ".zip"),
      
      # Set the content of the export
      content = function(file) {
        
        # Export figures with a progress bar
        shiny::withProgress(message = "Preparing figures for export...", value = 0, style = "old", {
          
          # Increase progress bar
          shiny::incProgress(amount = 1/5)
          
          # Set the working directory as a temporary location before zipping
          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          
          if(input$eco_fig_export == "all") {
            
            shinyalert::shinyalert("Note!", "Ecosystems not present in document(s) are not included in export file.", type = "warning")
            
            has_data(mean_ffreq() |>
                       dplyr::mutate(DR_Eco_SubClass = as.character(DR_Eco_SubClass)) |>
                       dplyr::group_by(DR_Eco_SubClass) |>
                       dplyr::summarise(MaxVal = max(Mean_FEGS_Score, na.rm = TRUE)) |>
                       dplyr::ungroup() |>
                       dplyr::filter(MaxVal > 0))
            
            # Create a vector of names for figures
            file_names <- c(paste0("Ecosystem_Frequencies_Bar_", Sys.Date(), ".jpg"),
                            paste0("Ecosystem_Frequencies_Pie_", Sys.Date(), ".jpg"),
                            paste0(stringr::str_replace_all(names(ben_bar_list()[names(ben_bar_list()) %in% has_data()$DR_Eco_SubClass]), " ", "_"),
                                   "_Ben_Bars_", Sys.Date(), ".jpg"),
                            paste0(stringr::str_replace_all(names(ben_pie_list()[names(ben_pie_list()) %in% has_data()$DR_Eco_SubClass]), " ", "_"),
                                   "_Ben_Pie_", Sys.Date(), ".jpg"),
                            paste0(stringr::str_replace_all(names(fegs_bar_list()[names(fegs_bar_list()) %in% has_data()$DR_Eco_SubClass]), " ", "_"),
                                   "_FEGS_Bars_", Sys.Date(), ".jpg"),
                            paste0(stringr::str_replace_all(names(fegs_pie_list()[names(fegs_pie_list()) %in% has_data()$DR_Eco_SubClass]), " ", "_"),
                                   "_FEGS_Pie_", Sys.Date(), ".jpg"))
            
            # Save ecosystem frequency figure (bar)
            ggplot2::ggsave(paste0("Ecosystem_Frequencies_Bar_", Sys.Date(), ".jpg"),
                            plot = eco_bar_fig(),
                            device = "jpg", width = 900, height = 900, units = "px", dpi = 100)
            
            # Save ecosystem frequency figure (pie)
            ggplot2::ggsave(paste0("Ecosystem_Frequencies_Pie_", Sys.Date(), ".jpg"),
                            plot = eco_pie_fig(),
                            device = "jpg", width = 900, height = 900, units = "px", dpi = 100)
            
            # Increase progress bar
            shiny::incProgress(amount = 2/5)
            
            # Save beneficiary bar charts
            for(n in names(ben_bar_list()[names(ben_bar_list()) %in% has_data()$DR_Eco_SubClass])) {
              ggplot2::ggsave(paste0(stringr::str_replace_all(n, " ", "_"), "_Ben_Bars_", Sys.Date(), ".jpg"),
                              ben_bar_list()[[n]],
                              device = "jpg",
                              width = 1200, height = 1200, units = "px", dpi = 100)
            }
            
            # Increase progress bar
            shiny::incProgress(amount = 3/5)
            
            # Save beneficiary pie charts
            for(n in names(ben_pie_list()[names(ben_pie_list()) %in% has_data()$DR_Eco_SubClass])) {
              ggplot2::ggsave(paste0(stringr::str_replace_all(n, " ", "_"), "_Ben_Pie_", Sys.Date(), ".jpg"),
                              ben_pie_list()[[n]],
                              device = "jpg",
                              width = 850, height = 850, units = "px", dpi = 100)
            }
            
            # Increase progress bar
            shiny::incProgress(amount = 4/5)
            
            # Save services bar charts
            for(n in names(fegs_bar_list()[names(fegs_bar_list()) %in% has_data()$DR_Eco_SubClass])) {
              ggplot2::ggsave(paste0(stringr::str_replace_all(n, " ", "_"), "_FEGS_Bars_", Sys.Date(), ".jpg"),
                              fegs_bar_list()[[n]],
                              device = "jpg",
                              width = 1200, height = 1400, units = "px", dpi = 100)
            }
            
            # Increase progress bar
            shiny::incProgress(amount = 5/5)
            
            # Save services pie charts
            for(n in names(fegs_pie_list()[names(fegs_pie_list()) %in% has_data()$DR_Eco_SubClass])) {
              ggplot2::ggsave(paste0(stringr::str_replace_all(n, " ", "_"), "_FEGS_Pie_", Sys.Date(), ".jpg"),
                              fegs_pie_list()[[n]],
                              device = "jpg",
                              width = 850, height = 850, units = "px", dpi = 100)
            }
            
            # Add the figures to the .zip file and save
            zip(file, files = file_names)
            
            # Show an alert when the process has completed
            shinyWidgets::show_alert(title = "Figures saved!",
                                     type = "success")
            
          } else {
            
            # Create a vector of names for figures
            file_names <- c(paste0("Ecosystem_Frequencies_Bar_", Sys.Date(), ".jpg"),
                            paste0("Ecosystem_Frequencies_Pie_", Sys.Date(), ".jpg"),
                            paste0(stringr::str_replace_all(names(ben_bar_list()[names(ben_bar_list()) %in% input$eco_fig_sel]), " ", "_"), "_Ben_Bars_", Sys.Date(), ".jpg"),
                            paste0(stringr::str_replace_all(names(ben_pie_list()[names(ben_pie_list()) %in% input$eco_fig_sel]), " ", "_"), "_Ben_Pie_", Sys.Date(), ".jpg"),
                            paste0(stringr::str_replace_all(names(fegs_bar_list()[names(fegs_bar_list()) %in% input$eco_fig_sel]), " ", "_"), "_FEGS_Bars_", Sys.Date(), ".jpg"),
                            paste0(stringr::str_replace_all(names(fegs_pie_list()[names(fegs_pie_list()) %in% input$eco_fig_sel]), " ", "_"), "_FEGS_Pie_", Sys.Date(), ".jpg"))
            
            # Save ecosystem frequency figure (bar)
            ggplot2::ggsave(paste0("Ecosystem_Frequencies_Bar_", Sys.Date(), ".jpg"),
                            plot = eco_bar_fig(),
                            device = "jpg", width = 900, height = 900, units = "px", dpi = 100)
            
            # Save ecosystem frequency figure (pie)
            ggplot2::ggsave(paste0("Ecosystem_Frequencies_Pie_", Sys.Date(), ".jpg"),
                            plot = eco_pie_fig(),
                            device = "jpg", width = 900, height = 900, units = "px", dpi = 100)
            
            # Increase progress bar
            shiny::incProgress(amount = 2/5)
            
            # Save beneficiary bar charts for selected ecosystems
            for(n in names(ben_bar_list()[names(ben_bar_list()) %in% input$eco_fig_sel])) {
              ggplot2::ggsave(paste0(stringr::str_replace_all(n, " ", "_"), "_Ben_Bars_", Sys.Date(), ".jpg"),
                              ben_bar_list()[[n]],
                              device = "jpg",
                              width = 1200, height = 1200, units = "px", dpi = 100)
            }
            
            # Increase progress bar
            shiny::incProgress(amount = 3/5)
            
            # Save beneficiary pie charts for selected ecosystems
            for(n in names(ben_pie_list()[names(ben_pie_list()) %in% input$eco_fig_sel])) {
              ggplot2::ggsave(paste0(stringr::str_replace_all(n, " ", "_"), "_Ben_Pie_", Sys.Date(), ".jpg"),
                              ben_pie_list()[[n]],
                              device = "jpg",
                              width = 850, height = 850, units = "px", dpi = 100)
            }
            
            # Increase progress bar
            shiny::incProgress(amount = 4/5)
            
            # Save services bar charts for selected ecosystems
            for(n in names(fegs_bar_list()[names(fegs_bar_list()) %in% input$eco_fig_sel])) {
              ggplot2::ggsave(paste0(stringr::str_replace_all(n, " ", "_"), "_FEGS_Bars_", Sys.Date(), ".jpg"),
                              fegs_bar_list()[[n]],
                              device = "jpg",
                              width = 1200, height = 1400, units = "px", dpi = 100)
            }
            
            # Increase progress bar
            shiny::incProgress(amount = 5/5)
            
            # Save services pie charts for selected ecosystems
            for(n in names(fegs_pie_list()[names(fegs_pie_list()) %in% input$eco_fig_sel])) {
              ggplot2::ggsave(paste0(stringr::str_replace_all(n, " ", "_"), "_FEGS_Pie_", Sys.Date(), ".jpg"),
                              fegs_pie_list()[[n]],
                              device = "jpg",
                              width = 850, height = 850, units = "px", dpi = 100)
            }
            
            # Add the figures to the .zip file and save
            zip(file, files = file_names)
            
            # Show an alert when the process has completed
            shinyWidgets::show_alert(title = "Figures saved!",
                                     type = "success")
            
          } # Close if else
          
        })  # Close withProgress
        
      }
      
    ) # Close downloadHandler
    
  } # Close server
  
  
  # Run the app -----
  shiny::shinyApp(ui, server)
}