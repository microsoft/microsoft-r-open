using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;

namespace RInstaller.views.Pages
{
    public interface IRInstallerPage
    {
        string RightButtonText { get; }

        string LeftButtonText { get; }

        Visibility RightButtonVisibility { get; }

        Visibility LeftButtonVisiblity { get; }

        /// <summary>
        /// Navigates to the next logical page if available
        /// </summary>
        /// <returns>Reference to the page navigated to</returns>
        IRInstallerPage NavigateNext();

        /// <summary>
        /// Navigates to the previous logical page if available
        /// </summary>
        /// <returns>Reference to the page navigated to</returns>
        IRInstallerPage NavigatePrevious();
    }
}
