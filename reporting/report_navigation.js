// =============================================================================
// ENEMDU technical report navigation
// Active stage / section tracking for the documentation sidebar
// =============================================================================

document.addEventListener("DOMContentLoaded", () => {

  // ---------------------------------------------------------------------------
  // 1. Navigation map
  //
  // Keep this map synchronized with the explicit IDs used in the QMD report.
  // New stages can be added here as the technical report grows.
  // ---------------------------------------------------------------------------

  const navigationMap = [
    {
      stageId: "etapa-01",
      stageLabel: "Etapa 01 — Ingesta ENEMDU",
      sections: [
        "resumen-de-la-base-importada",
        "variables-requeridas-para-el-procesamiento"
      ]
    },
    {
      stageId: "etapa-02",
      stageLabel: "Etapa 02 — Identificadores ENEMDU",
      sections: [
        "resumen-de-identificadores-construidos",
        "distribucion-por-dominio-de-estudio"
      ]
    }
  ];


  // ---------------------------------------------------------------------------
  // 2. Configuration
  //
  // A heading is considered active shortly before reaching the top of the
  // viewport. This produces a more natural reading / navigation behavior.
  // ---------------------------------------------------------------------------

  const activationOffset = 140;


  // ---------------------------------------------------------------------------
  // 3. Utility functions
  // ---------------------------------------------------------------------------

  const normalizeText = (text) => {
    return (text || "")
      .replace(/\s+/g, " ")
      .trim();
  };


  const getSidebar = () => {
    return document.querySelector(".sidebar-navigation");
  };


  const getHeading = (id) => {
    return document.getElementById(id);
  };


  const getLinkByHash = (id) => {

    const sidebar = getSidebar();

    if (!sidebar) {
      return null;
    }

    const links = Array.from(
      sidebar.querySelectorAll("a[href]")
    );

    return links.find((link) => {

      try {

        const url = new URL(
          link.href,
          window.location.href
        );

        return url.hash === `#${id}`;

      } catch {

        return false;
      }
    }) || null;
  };


  const getStageLabelElement = (label) => {

    const sidebar = getSidebar();

    if (!sidebar) {
      return null;
    }

    const candidates = Array.from(
      sidebar.querySelectorAll(
        ".menu-text, .sidebar-item-text, a, span"
      )
    );

    return candidates.find((element) => {
      return normalizeText(element.textContent) === normalizeText(label);
    }) || null;
  };


  const getStageContainer = (labelElement) => {

    if (!labelElement) {
      return null;
    }

    return (
      labelElement.closest(".sidebar-item-container") ||
      labelElement.closest(".sidebar-item-section") ||
      labelElement.parentElement
    );
  };


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
  // 5. Determine current stage and technical output
  // ---------------------------------------------------------------------------

  const getCurrentNavigationState = () => {

    const headings = [];

    navigationMap.forEach((stage) => {

      const stageHeading = getHeading(stage.stageId);

      if (stageHeading) {
        headings.push({
          id: stage.stageId,
          stageId: stage.stageId,
          type: "stage",
          element: stageHeading
        });
      }

      stage.sections.forEach((sectionId) => {

        const sectionHeading = getHeading(sectionId);

        if (sectionHeading) {
          headings.push({
            id: sectionId,
            stageId: stage.stageId,
            type: "section",
            element: sectionHeading
          });
        }
      });
    });


    if (headings.length === 0) {
      return null;
    }


    // Preserve document order.
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


    // Default to Stage 01 while the reader is still above its heading.
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


    const stageDefinition = navigationMap.find(
      (stage) => stage.stageId === current.stageId
    );


    // Highlight current stage.
    if (stageDefinition) {

      const stageLabelElement =
        getStageLabelElement(
          stageDefinition.stageLabel
        );

      const stageContainer =
        getStageContainer(
          stageLabelElement
        );


      if (stageLabelElement) {
        stageLabelElement.classList.add(
          "report-nav-stage-active"
        );
      }


      if (stageContainer) {
        stageContainer.classList.add(
          "report-nav-stage-active"
        );
      }
    }


    // Highlight current subsection when applicable.
    if (current.type === "section") {

      const sectionLink =
        getLinkByHash(current.id);


      if (sectionLink) {

        sectionLink.classList.add(
          "report-nav-item-active"
        );


        const menuText =
          sectionLink.querySelector(".menu-text");


        if (menuText) {
          menuText.classList.add(
            "report-nav-item-active"
          );
        }
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