// =============================================================================
// Common report navigation
// Active stage / section tracking for accumulated Quarto reports
// =============================================================================

document.addEventListener("DOMContentLoaded", () => {

  // ---------------------------------------------------------------------------
  // 1. Base elements and configuration
  // ---------------------------------------------------------------------------

  const sidebar = document.querySelector(".sidebar-navigation");
  const reportMain = document.querySelector("main.content");

  if (!sidebar || !reportMain) {
    return;
  }

  // A heading is considered active shortly before reaching the top of the
  // viewport. This keeps navigation behavior stable while scrolling.
  const activationOffset = 140;


  // ---------------------------------------------------------------------------
  // 2. Resolve sidebar links from report section IDs
  // ---------------------------------------------------------------------------

  const sidebarLinks = Array.from(
    sidebar.querySelectorAll("a[href]")
  );

  const getLinkByHash = (id) => {

    return sidebarLinks.find((link) => {

      try {

        const url = new URL(
          link.href,
          window.location.href
        );

        return decodeURIComponent(url.hash) === `#${id}`;

      } catch {

        return false;
      }
    }) || null;
  };


  // ---------------------------------------------------------------------------
  // 3. Build navigation map dynamically from the rendered report
  // ---------------------------------------------------------------------------
  // No stage IDs, labels, or subsection IDs are hardcoded here.
  //
  // A stage is a level-1 report section. Its navigable subsections are the
  // direct level-2 children that also have a link in the Quarto sidebar.
  // Therefore, adding a new stage only requires adding it to the report and
  // profile; this script discovers it automatically.

  const navigationMap = Array.from(
    reportMain.querySelectorAll("section.level1[id]")
  )
    .map((stageElement) => {

      const sections = Array.from(stageElement.children)
        .filter((element) => {
          return element.matches("section.level2[id]");
        })
        .map((sectionElement) => {

          const link = getLinkByHash(sectionElement.id);

          return {
            id: sectionElement.id,
            element: sectionElement,
            link: link
          };
        })
        .filter((section) => section.link !== null);


      const firstSectionLink =
        sections.length > 0 ? sections[0].link : null;

      const sidebarStageItem = firstSectionLink
        ? firstSectionLink.closest(".sidebar-item-section")
        : null;


      return {
        stageId: stageElement.id,
        element: stageElement,
        sections: sections,
        sidebarStageItem: sidebarStageItem
      };
    })
    .filter((stage) => {
      return stage.sidebarStageItem !== null;
    });


  if (navigationMap.length === 0) {
    return;
  }


  // ---------------------------------------------------------------------------
  // 4. Remove navigation states assigned by this script
  // ---------------------------------------------------------------------------

  const clearReportNavigationState = () => {

    document
      .querySelectorAll(
        ".report-nav-stage-active, .report-nav-item-active"
      )
      .forEach((element) => {
        element.classList.remove(
          "report-nav-stage-active",
          "report-nav-item-active"
        );
      });
  };


  // ---------------------------------------------------------------------------
  // 5. Determine current stage and visible report output
  // ---------------------------------------------------------------------------

  const getCurrentNavigationState = () => {

    const headings = [];


    navigationMap.forEach((stage) => {

      headings.push({
        id: stage.stageId,
        stage: stage,
        type: "stage",
        element: stage.element,
        link: null
      });


      stage.sections.forEach((section) => {

        headings.push({
          id: section.id,
          stage: stage,
          type: "section",
          element: section.element,
          link: section.link
        });
      });
    });


    // Preserve document order independently of the order declared in the
    // Quarto sidebar.
    headings.sort((a, b) => {

      const position =
        a.element.compareDocumentPosition(b.element);

      if (
        position &
        Node.DOCUMENT_POSITION_FOLLOWING
      ) {
        return -1;
      }

      if (
        position &
        Node.DOCUMENT_POSITION_PRECEDING
      ) {
        return 1;
      }

      return 0;
    });


    // While the reader is above the first stage heading, keep the first stage
    // as the current navigation context.
    let current = headings[0];


    for (const heading of headings) {

      const top =
        heading.element.getBoundingClientRect().top;

      if (top <= activationOffset) {
        current = heading;
      } else {
        break;
      }
    }


    return current;
  };


  // ---------------------------------------------------------------------------
  // 6. Apply active state to sidebar
  // ---------------------------------------------------------------------------

  const updateNavigation = () => {

    const current = getCurrentNavigationState();

    if (!current) {
      return;
    }


    clearReportNavigationState();


    // Highlight current stage using the sidebar group that contains its first
    // navigable subsection. No stage label comparison is required.
    const stageItem = current.stage.sidebarStageItem;

    if (stageItem) {

      const stageContainer =
        stageItem.querySelector(":scope > .sidebar-item-container") ||
        stageItem;

      const stageLabel =
        stageContainer.querySelector(".menu-text") ||
        stageContainer.querySelector(".sidebar-item-text");


      stageContainer.classList.add(
        "report-nav-stage-active"
      );


      if (stageLabel) {
        stageLabel.classList.add(
          "report-nav-stage-active"
        );
      }
    }


    // Highlight current subsection when applicable.
    if (
      current.type === "section" &&
      current.link
    ) {

      current.link.classList.add(
        "report-nav-item-active"
      );


      const menuText =
        current.link.querySelector(".menu-text");


      if (menuText) {
        menuText.classList.add(
          "report-nav-item-active"
        );
      }
    }
  };


  // ---------------------------------------------------------------------------
  // 7. Efficient scroll handling
  // ---------------------------------------------------------------------------

  let ticking = false;


  const requestNavigationUpdate = () => {

    if (ticking) {
      return;
    }


    window.requestAnimationFrame(() => {

      updateNavigation();

      ticking = false;
    });


    ticking = true;
  };


  window.addEventListener(
    "scroll",
    requestNavigationUpdate,
    { passive: true }
  );


  window.addEventListener(
    "resize",
    requestNavigationUpdate,
    { passive: true }
  );


  window.addEventListener(
    "hashchange",
    requestNavigationUpdate
  );


  // ---------------------------------------------------------------------------
  // 8. Initial state
  // ---------------------------------------------------------------------------

  updateNavigation();

});
